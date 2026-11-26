-- | LSP server skeleton.
--
-- All Agda-specific work (loading files, collecting goals, highlighting,
-- diagnostics) is meant to live here, so editor plugins stay thin.
--
-- Files are type-checked by one @agda --interaction-json@ process
-- ("Agdalicious.Agda"); 'load' turns its errors and warnings into diagnostics.
-- Agda checks the editor's contents, not the saved files: see "Agdalicious.Shadow".
module Agdalicious.Server (run) where

import Agdalicious.Agda (Agda)
import Agdalicious.Agda qualified as Agda
import Agdalicious.Infoview
import Agdalicious.Shadow (Shadow)
import Agdalicious.Shadow qualified as Shadow
import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.MVar
import Control.Lens ((^.), (^..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (withRunInIO)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key, values, _String)
import Data.Foldable (for_)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Language.LSP.Protocol.Lens qualified as L
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Language.LSP.VFS (vfsMap, virtualFileText)
import Text.Read (readMaybe)

type InfoviewMethod = 'Method_CustomMethod "agda/infoview" :: Method 'ClientToServer 'Request

run :: IO Int
run = Shadow.with $ \shadow -> do
  st <- State <$> Agda.start <*> pure shadow <*> newMVar mempty
  runServer
    ServerDefinition
      { parseConfig = \cfg _ -> Right cfg
      , onConfigChange = const (pure ())
      , defaultConfig = ()
      , configSection = "agdalicious"
      , doInitialize = \env _req -> pure (Right env)
      , staticHandlers = const handlers
      , interpretHandler = \env -> Iso (runLspT env . flip runReaderT st) liftIO
      , options = defaultOptions {optTextDocumentSync = Just syncOptions}
      }

syncOptions :: TextDocumentSyncOptions
syncOptions =
  TextDocumentSyncOptions
    (Just True) -- openClose
    (Just TextDocumentSyncKind_Incremental) -- change
    Nothing -- willSave
    Nothing -- willSaveWaitUntil
    (Just (InR (SaveOptions (Just False)))) -- save

-- | What the handlers share.
data State = State
  { agda :: Agda
  , shadow :: Shadow
  -- ^ the mirror Agda reads documents from
  , running :: MVar (Map.Map Uri (Async ()))
  -- ^ the load currently running for each document
  }

-- | The monad the handlers run in: the LSP monad, with the shared state.
type ServerM = ReaderT State (LspM ())

handlers :: Handlers ServerM
handlers =
  mconcat
    [ notificationHandler SMethod_Initialized $ \_ ->
        logInfo "agdalicious-lsp initialized"
    , notificationHandler SMethod_TextDocumentDidOpen $ \msg ->
        edited (msg ^. L.params . L.textDocument . L.uri)
    , notificationHandler SMethod_TextDocumentDidChange $ \msg ->
        edited (msg ^. L.params . L.textDocument . L.uri)
    , notificationHandler SMethod_TextDocumentDidSave $ \_ ->
        pure () -- Agda already checked these contents
    , notificationHandler SMethod_TextDocumentDidClose $ \msg -> do
        -- unsaved changes are gone: the files importing it go back to the saved one
        let uri = msg ^. L.params . L.textDocument . L.uri
        s <- asks shadow
        for_ (uriToFilePath uri) (liftIO . Shadow.restore s)
        reloadOpen uri
    , notificationHandler SMethod_SetTrace $ \_ ->
        pure ()
    , requestHandler SMethod_TextDocumentSemanticTokensFull $ \_ respond ->
        -- TODO: Agda's highlighting information
        respond (Right (InR Null))
    , requestHandler (SMethod_CustomMethod (Proxy @"agda/infoview") :: SMethod InfoviewMethod) $
        \req respond ->
          case Aeson.fromJSON (req ^. L.params) of
            Aeson.Error err ->
              respond . Left $
                TResponseError (InR ErrorCodes_InvalidParams) (T.pack err) Nothing
            Aeson.Success params ->
              respond . Right . Aeson.toJSON =<< infoview params
    ]

-- | A document was opened or edited: put its contents in the mirror and check
-- it, then the other open documents, as they may import it. Agda only
-- re-checks the modules whose sources (or imports) changed, so this is cheap
-- for documents that don't depend on it.
edited :: Uri -> ServerM ()
edited uri = do
  s <- asks shadow
  file <- getVirtualFile (toNormalizedUri uri)
  for_ ((,) <$> uriToFilePath uri <*> file) $ \(f, vf) ->
    liftIO (Shadow.write s f (virtualFileText vf))
  reloadOpen uri

-- | Reload every open document, starting with @uri@ if it is one of them.
reloadOpen :: Uri -> ServerM ()
reloadOpen uri = do
  open <- Map.keys . (^. vfsMap) <$> getVirtualFiles
  for_ (sortOn (/= toNormalizedUri uri) open) (reload . fromNormalizedUri)

-- | Type-check a document's copy in the mirror and publish its errors and
-- warnings as diagnostics.
load :: Uri -> ServerM ()
load uri = for_ (uriToFilePath uri) $ \file -> do
  State {agda, shadow} <- asks id
  let copy = Shadow.path shadow file
  responses <- liftIO (Agda.command agda copy ("Cmd_load " <> show copy <> " []"))
  sendNotification SMethod_TextDocumentPublishDiagnostics . PublishDiagnosticsParams uri Nothing $
    map (diagnostic file . fmap (Shadow.unshadow shadow)) (concatMap messagesOf responses)

-- | The errors and warnings of a @DisplayInfo@ response (@Error@ or @AllGoalsWarnings@).
messagesOf :: Aeson.Value -> [(DiagnosticSeverity, T.Text)]
messagesOf r =
  map (DiagnosticSeverity_Error,) (info (key "error" . key "message") <> info (key "errors" . values . key "message"))
    <> map (DiagnosticSeverity_Warning,) (info (key "warnings" . values . key "message"))
  where
    info l = r ^.. key "info" . l . _String

-- | Agda's messages start with their location: @/x/A.agda:11.5-8: error: …@.
-- Those located in another file (an import that fails) go on the first line.
diagnostic :: FilePath -> (DiagnosticSeverity, T.Text) -> Diagnostic
diagnostic file (sev, msg) = case T.splitOn ":" msg of
  p : loc : rest | p == T.pack file, Just r <- range loc -> mk r (T.strip (T.intercalate ":" rest))
  _ -> mk (Range (Position 0 0) (Position 0 0)) msg
  where
    mk r m = Diagnostic r (Just sev) Nothing Nothing (Just "agda") m Nothing Nothing Nothing

-- | @11.5@, @11.5-8@ or @11.5-12.3@ (1-based, end exclusive).
range :: T.Text -> Maybe Range
range t = case map (traverse (readMaybe . T.unpack) . T.splitOn ".") (T.splitOn "-" t) of
  [Just [l, c]] -> pos l c l c
  [Just [l, c], Just [c']] -> pos l c l c'
  [Just [l, c], Just [l', c']] -> pos l c l' c'
  _ -> Nothing
  where
    pos l c l' c' = Just (Range (Position (l - 1) (c - 1)) (Position (l' - 1) (c' - 1)))

-- | Load a file in the background, cancelling a load of it that is still
-- running, so only the newest contents get checked.
reload :: Uri -> ServerM ()
reload uri = do
  r <- asks running
  withRunInIO $ \inIO -> modifyMVar_ r $ \m -> do
    new <- async (for_ (Map.lookup uri m) cancel >> inIO (load uri))
    pure (Map.insert uri new m)

-- | Compute the infoview contents at a position. Placeholder for now.
infoview :: InfoviewParams -> ServerM InfoviewResult
infoview InfoviewParams {textDocument, position} =
  pure
    InfoviewResult
      { goals = []
      , messages =
          [ "agdalicious-lsp: infoview not implemented yet"
          , "file: " <> getUri (textDocument ^. L.uri)
          , "line " <> tshow (position ^. L.line + 1) <> ", col " <> tshow (position ^. L.character + 1)
          ]
      }
  where
    tshow = T.pack . show

logInfo :: T.Text -> ServerM ()
logInfo t =
  sendNotification SMethod_WindowLogMessage (LogMessageParams MessageType_Info t)
