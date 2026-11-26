-- | LSP server skeleton.
--
-- All Agda-specific work (loading files, collecting goals, highlighting,
-- diagnostics) is meant to live here, so editor plugins stay thin.
--
-- Files are type-checked by one @agda --interaction-json@ process
-- ("Agdalicious.Agda"); 'load' turns its errors and warnings into diagnostics.
module Agdalicious.Server (run) where

import Agdalicious.Agda (Agda)
import Agdalicious.Agda qualified as Agda
import Agdalicious.Infoview
import Control.Concurrent (forkIO)
import Control.Lens ((^.), (^..))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key, values, _String)
import Data.Foldable (for_)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Language.LSP.Protocol.Lens qualified as L
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Text.Read (readMaybe)

type InfoviewMethod = 'Method_CustomMethod "agda/infoview" :: Method 'ClientToServer 'Request

run :: IO Int
run = do
  agda <- Agda.start
  runServer
    ServerDefinition
      { parseConfig = \cfg _ -> Right cfg
      , onConfigChange = const (pure ())
      , defaultConfig = ()
      , configSection = "agdalicious"
      , doInitialize = \env _req -> pure (Right env)
      , staticHandlers = const (handlers agda)
      , interpretHandler = \env -> Iso (runLspT env) liftIO
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

handlers :: Agda -> Handlers (LspM ())
handlers agda =
  mconcat
    [ notificationHandler SMethod_Initialized $ \_ ->
        logInfo "agdalicious-lsp initialized"
    , notificationHandler SMethod_TextDocumentDidOpen $ \msg ->
        background (load agda (msg ^. L.params . L.textDocument . L.uri))
    , notificationHandler SMethod_TextDocumentDidChange $ \_ ->
        pure () -- the VFS tracks contents for us
    , notificationHandler SMethod_TextDocumentDidSave $ \msg ->
        -- TODO: reload the file in Agda, publish diagnostics
        logInfo ("saved " <> getUri (msg ^. L.params . L.textDocument . L.uri))
    , notificationHandler SMethod_TextDocumentDidClose $ \_ ->
        pure ()
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

-- | Type-check a file and publish its errors and warnings as diagnostics.
load :: Agda -> Uri -> LspM () ()
load agda uri = for_ (uriToFilePath uri) $ \file -> do
  responses <- liftIO (Agda.command agda file ("Cmd_load " <> show file <> " []"))
  sendNotification SMethod_TextDocumentPublishDiagnostics $
    PublishDiagnosticsParams uri Nothing (concatMap diagnostics responses)

-- | The errors and warnings of a @DisplayInfo@ response (@Error@ or @AllGoalsWarnings@).
diagnostics :: Aeson.Value -> [Diagnostic]
diagnostics r =
  map (diagnostic DiagnosticSeverity_Error) (info (key "error" . key "message") <> info (key "errors" . values . key "message"))
    <> map (diagnostic DiagnosticSeverity_Warning) (info (key "warnings" . values . key "message"))
  where
    info l = r ^.. key "info" . l . _String

-- | Agda's messages start with their location: @/x/A.agda:11.5-8: error: …@.
diagnostic :: DiagnosticSeverity -> T.Text -> Diagnostic
diagnostic sev msg = case T.splitOn ":" msg of
  _ : loc : rest | Just r <- range loc -> mk r (T.strip (T.intercalate ":" rest))
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

-- | Run off the reactor thread, so a long check doesn't block other requests.
background :: LspM () () -> LspM () ()
background act = getLspEnv >>= \env -> liftIO (void (forkIO (runLspT env act)))

-- | Compute the infoview contents at a position. Placeholder for now.
infoview :: InfoviewParams -> LspM () InfoviewResult
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

logInfo :: T.Text -> LspM () ()
logInfo t =
  sendNotification SMethod_WindowLogMessage (LogMessageParams MessageType_Info t)
