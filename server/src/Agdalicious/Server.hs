-- | LSP server skeleton.
--
-- All Agda-specific work (loading files, collecting goals, highlighting,
-- diagnostics) is meant to live here, so editor plugins stay thin.
--
-- Files are type-checked by one @agda --interaction-json@ process
-- ("Agdalicious.Agda"); 'load' turns its errors and warnings into diagnostics,
-- and keeps what its highlighting says about the document
-- ("Agdalicious.Document"), for going to definitions and hovering. Hover texts
-- come from Agda, the sources of the definitions ("Agdalicious.Source") and
-- the Agda user manual ("Agdalicious.Manual").
-- Agda checks the editor's contents, not the saved files: see "Agdalicious.Shadow".
module Agdalicious.Server (run) where

import Agdalicious.Agda (Agda)
import Agdalicious.Agda qualified as Agda
import Agdalicious.Document (Document, Goal (..), Token (..), goalAt, offsetOf, positionOf, rangeOf, slice, tokenAt)
import Agdalicious.Document qualified as Doc
import Agdalicious.Hover (Info (..), render)
import Agdalicious.Manual qualified as Manual
import Agdalicious.Source qualified as Source
import Agdalicious.Infoview (InfoviewParams (..), InfoviewResult (..))
import Agdalicious.Shadow (Shadow)
import Agdalicious.Shadow qualified as Shadow
import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.MVar
import Control.Lens ((^.), (^..), (^?))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (withRunInIO)
import Control.Monad (guard, unless)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), hoistMaybe, runMaybeT)
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key, values, _String)
import Data.Bifunctor (first)
import Data.Foldable (for_)
import Data.List (nub, sortOn)
import Data.Maybe (catMaybes, listToMaybe, mapMaybe, maybeToList)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Text.IO.Utf8 qualified as T
import Data.Text.Mixed.Rope (Rope)
import Data.Text.Mixed.Rope qualified as Rope
import Language.LSP.Protocol.Lens qualified as L
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Language.LSP.VFS (file_text, vfsMap, virtualFileText)
import Text.Read (readMaybe)

type InfoviewMethod = 'Method_CustomMethod "agda/infoview" :: Method 'ClientToServer 'Request

run :: IO Int
run = Shadow.with $ \shadow -> do
  st <- State <$> Agda.start <*> pure shadow <*> newMVar mempty <*> newMVar mempty
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
  , documents :: MVar (Map.Map Uri Document)
  -- ^ what each document's last load said about it
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
    , requestHandler SMethod_TextDocumentDefinition $ \req respond -> do
        let p = req ^. L.params
        loc <- definition (p ^. L.textDocument . L.uri) (p ^. L.position)
        respond (Right (maybe (InR (InR Null)) (InL . Definition . InL) loc))
    , requestHandler SMethod_TextDocumentHover $ \req respond -> do
        let p = req ^. L.params
        h <- hover (p ^. L.textDocument . L.uri) (p ^. L.position)
        respond (Right (maybe (InR Null) InL h))
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
  State {agda, shadow, documents} <- asks id
  let copy = Shadow.path shadow file
  responses <- liftIO (Agda.command agda copy ("Cmd_load " <> show copy <> " []"))
  -- a load that fails early has no highlighting: keep what the previous one said then
  let doc = Doc.fromResponses responses
      unshadow t = t {site = first (T.unpack . Shadow.unshadow shadow . T.pack) <$> site t}
  unless (null (Doc.tokens doc)) . liftIO $
    modifyMVar_ documents (pure . Map.insert uri doc {Doc.tokens = map unshadow (Doc.tokens doc)})
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

-- | A document's text, the offset of a position in it, and what its last load said.
at :: Uri -> Position -> MaybeT ServerM (Rope, Int, Document)
at uri pos = do
  text <- MaybeT (fmap (^. file_text) <$> getVirtualFile (toNormalizedUri uri))
  off <- hoistMaybe (offsetOf pos text)
  doc <- MaybeT (Map.lookup uri <$> (liftIO . readMVar =<< asks documents))
  pure (text, off, doc)

-- | Where the name at a position is defined, as of the document's last load.
definition :: Uri -> Position -> ServerM (Maybe Location)
definition uri pos = runMaybeT $ do
  (_, off, doc) <- at uri pos
  (file, o) <- hoistMaybe (site =<< tokenAt off (Doc.tokens doc))
  p <- positionOf o <$> lift (textOf file)
  pure (Location (filePathToUri file) (Range p p))

-- | What there is to know about the token (or hole) at a position.
hover :: Uri -> Position -> ServerM (Maybe Hover)
hover uri pos = runMaybeT $ do
  (text, off, doc) <- at uri pos
  (f, t, info) <- case goalAt off (Doc.goals doc) of
    Just g -> (goalFrom g,goalTo g,) <$> lift (goalInfo uri g)
    Nothing -> do
      tok <- hoistMaybe (tokenAt off (Doc.tokens doc))
      (from tok,to tok,) <$> lift (tokenInfo uri (slice (from tok) (to tok) text) tok)
  let md = render info
  guard (not (T.null md))
  pure (Hover (InL (MarkupContent MarkupKind_Markdown md)) (Just (rangeOf f t text)))

-- | A hole: its type, and what is in scope there.
goalInfo :: Uri -> Goal -> ServerM Info
goalInfo uri g = do
  rs <- query uri ("Cmd_goal_type_context Simplified " <> show (goal g) <> " noRange \"\"")
  pure
    mempty
      { signature = ["?" <> T.pack (show (goal g)) <> " : " <> ty | ty <- rs ^.. traverse . key "info" . key "goalInfo" . key "type" . _String]
      , code =
          [ n <> " : " <> b
          | e <- rs ^.. traverse . key "info" . key "goalInfo" . key "entries" . values
          , Just n <- [e ^? key "reifiedName" . _String]
          , Just b <- [e ^? key "binding" . _String]
          ]
      }

-- | A keyword, symbol, pragma or option is explained by the manual; a name by
-- its type, its definition and the comments in its source.
tokenInfo :: Uri -> T.Text -> Token -> ServerM Info
tokenInfo uri word tok = case site tok of
  Nothing -> pure (manualInfo word (listToMaybe (mapMaybe Manual.term [word, "BUILTIN " <> word])))
  Just (file, off) -> nameInfo uri word tok file off

manualInfo :: T.Text -> Maybe Manual.Entry -> Info
manualInfo word = maybe mempty $ \e ->
  Info
    { signature = []
    , summary = ["`" <> word <> "` — " <> Manual.summary e]
    , code = if T.null (Manual.example e) then [] else T.lines (Manual.example e)
    , links = [(Manual.title e, Manual.url e)]
    , details = [Manual.text e]
    }

nameInfo :: Uri -> T.Text -> Token -> FilePath -> Int -> ServerM Info
nameInfo uri word tok file off = do
  src <- textOf file
  lib <- liftIO (Source.libraryOf file)
  let ls = Rope.lines src
      line = fromIntegral (positionOf off src ^. L.line)
      name = Source.definedName off (Rope.toText src)
      decl = Source.declaration line ls
      modul = Source.moduleOf ls
      builtin = Source.builtinOf name ls >>= Manual.term
      kind k = k `elem` atoms tok
      -- as it is called here (ℕ, not Nat), unless it is part of an operator
      shown = if "_" `T.isInfixOf` name && word /= name then name else word
  ty <- if kind "bound" || kind "module" then pure Nothing else inferType uri (nub [word, name])
  let declared = [T.strip c | c : _ <- [Source.code decl], " : " `T.isInfixOf` c]
      signature
        | kind "module" = ["module " <> m | m <- maybeToList (Source.moduleName modul)]
        -- an inferred type with metavariables (for implicit arguments Agda
        -- inserted, as in @_x_15 ≡ _x_15@) says less than the declared one
        | Just t <- ty, not (hasMetas t) || null declared = [shown <> " : " <> t]
        | otherwise = declared
      (doc1, docRest) = splitAt 1 (Source.doc decl)
      summary
        | kind "module" = Source.header modul
        | kind "bound" = ["Bound at line " <> T.pack (show (line + 1)) <> "."]
        | not (null doc1) = doc1
        | otherwise = [Manual.summary e | e <- maybeToList builtin]
      code
        | kind "module" || kind "bound" = []
        | Source.complete decl && map T.strip (Source.code decl) /= signature = Source.code decl
        | otherwise = maybe [] (T.lines . Manual.example) builtin
      place = catMaybes [Source.moduleName modul, Source.section decl]
      libLink = do
        Source.Library n v <- lib
        m <- Source.moduleName modul
        Manual.libraryLink n v m off
      links =
        [(T.intercalate " › " place, u) | u <- maybeToList libLink]
          <> [(Manual.title e, Manual.url e) | e <- maybeToList builtin]
          <> [ (T.intercalate " › " place <> ":" <> T.pack (show (line + 1)), getUri (filePathToUri file))
             | Nothing <- [libLink]
             , not (kind "bound")
             ]
      details
        | kind "module" || kind "bound" = []
        | otherwise =
            docRest
              <> ["*" <> T.intercalate " › " place <> "*" | not (null place)]
              <> drop 1 (Source.header modul)
              <> [Manual.text e | e <- maybeToList builtin]
  pure Info {signature, summary, code, links, details}

-- | Whether a type has metavariables, which Agda prints as @_12@ or @_x_12@.
hasMetas :: T.Text -> Bool
hasMetas = any meta . T.split (\c -> c `elem` (" (){}" :: String))
  where
    meta w = case T.stripPrefix "_" w of
      Just r -> not (T.null r) && T.all (`elem` ("0123456789" :: String)) (T.takeWhileEnd (/= '_') r) && T.takeWhileEnd (/= '_') r /= ""
      Nothing -> False

-- | The type of the first of some names that Agda can infer one for, in the
-- scope of a document.
inferType :: Uri -> [T.Text] -> ServerM (Maybe T.Text)
inferType _ [] = pure Nothing
inferType uri (n : ns) = do
  rs <- query uri ("Cmd_infer_toplevel AsIs " <> show (T.unpack n))
  maybe (inferType uri ns) (pure . Just) (listToMaybe (rs ^.. traverse . key "info" . key "expr" . _String))

-- | Run a goal or top-level command in the scope of a document (its copy in
-- the mirror, which Agda loads first if needed).
query :: Uri -> String -> ServerM [Aeson.Value]
query uri cmd = do
  State {agda, shadow} <- asks id
  case uriToFilePath uri of
    Nothing -> pure []
    Just f -> liftIO (Agda.command agda (Shadow.path shadow f) cmd)

-- | A file's text: the editor's if it is open, else the saved one.
textOf :: FilePath -> ServerM Rope
textOf f =
  getVirtualFile (toNormalizedUri (filePathToUri f))
    >>= maybe (liftIO (Rope.fromText <$> T.readFile f)) (pure . (^. file_text))

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
