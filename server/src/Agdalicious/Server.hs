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
import Agdalicious.Completion (Context (..))
import Agdalicious.Completion qualified as Completion
import Agdalicious.Document (Document, Goal (..), Token (..), goalAt, offsetOf, positionOf, rangeOf, slice, tokenAt)
import Agdalicious.Document qualified as Doc
import Agdalicious.Hover (Info (..), render)
import Agdalicious.Manual qualified as Manual
import Agdalicious.Source qualified as Source
import Agdalicious.Infoview (InfoviewParams (..), InfoviewResult (..))
import Agdalicious.Libraries qualified as Libraries
import Agdalicious.Shadow (Shadow)
import Agdalicious.Shadow qualified as Shadow
import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.MVar
import Control.Lens ((&), (.~), (?~), (^.), (^..), (^?))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (withRunInIO)
import Control.Monad (guard, unless, void)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), hoistMaybe, runMaybeT)
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key, values, _String)
import Data.Bifunctor (first)
import Data.Foldable (for_)
import System.FilePath ((</>))
import Data.List (isPrefixOf, nub, sortOn)
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
  st <-
    State
      <$> Agda.start
      <*> pure shadow
      <*> newMVar mempty
      <*> newMVar mempty
      <*> Libraries.config
      <*> newMVar mempty
      <*> newMVar mempty
      <*> newMVar ()
  runServer
    ServerDefinition
      { parseConfig = \cfg _ -> Right cfg
      , onConfigChange = const (pure ())
      , defaultConfig = ()
      , configSection = "agdalicious"
      , doInitialize = \env _req -> pure (Right env)
      , staticHandlers = const handlers
      , interpretHandler = \env -> Iso (runLspT env . flip runReaderT st) liftIO
      , options =
          defaultOptions
            { optTextDocumentSync = Just syncOptions
            , optCompletionTriggerCharacters = Just ".-"
            }
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
  , libraryConfig :: Libraries.Config
  , moduleLists :: MVar (Map.Map FilePath [(T.Text, FilePath)])
  -- ^ the modules in each library directory
  , exported :: MVar (Map.Map T.Text [(T.Text, T.Text)])
  -- ^ the names (with types) each module exports, as far as asked for
  , scratch :: MVar ()
  -- ^ held while a scratch module (see 'exports') is in use
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
    , requestHandler SMethod_TextDocumentCompletion $ \req respond -> do
        let p = req ^. L.params
        items <- completion (p ^. L.textDocument . L.uri) (p ^. L.position)
        respond (Right (InL items))
    , requestHandler SMethod_CompletionItemResolve $ \req respond ->
        respond . Right =<< resolveItem (req ^. L.params)
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
  State {shadow, exported} <- asks id
  file <- getVirtualFile (toNormalizedUri uri)
  for_ ((,) <$> uriToFilePath uri <*> file) $ \(f, vf) -> liftIO $ do
    Shadow.write shadow f (virtualFileText vf)
    -- what its module exports may have changed
    for_ (Source.moduleName (Source.moduleOf (T.lines (virtualFileText vf)))) $ \m ->
      modifyMVar_ exported (pure . Map.delete m)
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

-- * Completion

-- | What can be written at a position: keywords, pragmas, options and
-- built-ins (documented from the manual), modules, and names.
completion :: Uri -> Position -> ServerM [CompletionItem]
completion uri pos@(Position l c) = fmap (maybe [] id) . runMaybeT $ do
  text <- MaybeT (fmap (^. file_text) <$> getVirtualFile (toNormalizedUri uri))
  before <- hoistMaybe (Rope.toText . fst <$> Rope.utf16SplitAt (fromIntegral c) (Rope.getLine (fromIntegral l) text))
  let edit w i = i & L.textEdit ?~ InL (TextEdit (Range (Position l (c - utf16Length w)) pos) (i ^. L.label))
      documented kind w = [edit w (manualItem k e kind) | (k, e) <- Manual.termsOf kind]
      docText = Rope.toText text
  lift $ case Completion.context before of
    Pragma w -> pure (documented "pragma" w)
    PragmaOption w -> pure (documented "pragma-option" w)
    Builtin w ->
      pure [edit w (manualItem (T.drop 8 k) e "builtin") | (k, e) <- Manual.termsOf "builtin"]
    ModulePath w -> do
      ms <- modules uri docText
      pure
        [ edit w (item m CompletionItemKind_Module & L.data_ ?~ Aeson.object ["file" Aeson..= f])
        | (m, f) <- ms
        ]
    ImportedName m w -> do
      names <- exports uri docText m
      pure [edit w (nameItem n t) | (n, t) <- names]
    Code w -> do
      names <- inScope uri text
      pure (documented "keyword" w <> [edit w (nameItem n t) | (n, t) <- names])
    NoCompletion -> pure []
  where
    utf16Length = fromIntegral . T.foldl' (\n ch -> n + if fromEnum ch > 0xFFFF then 2 else 1) (0 :: Int)

item :: T.Text -> CompletionItemKind -> CompletionItem
item label kind =
  CompletionItem label Nothing (Just kind) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | A keyword, pragma, option or built-in, with what the manual says about it.
manualItem :: T.Text -> Manual.Entry -> T.Text -> CompletionItem
manualItem label e kind =
  item label (if kind == "keyword" then CompletionItemKind_Keyword else CompletionItemKind_Property)
    & L.detail ?~ Manual.title e
    & L.documentation ?~ InR (MarkupContent MarkupKind_Markdown (render (manualInfo label (Just e)) {details = []}))

nameItem :: T.Text -> T.Text -> CompletionItem
nameItem n t = item n CompletionItemKind_Function & L.detail ?~ t

-- | A module's documentation (the comment at the top of its file), when a
-- completion of it is selected.
resolveItem :: CompletionItem -> ServerM CompletionItem
resolveItem i = case i ^? L.data_ . traverse . key "file" . _String of
  Nothing -> pure i
  Just f -> do
    src <- textOf (T.unpack f)
    let header = Source.header (Source.moduleOf (Rope.lines src))
    pure (if null header then i else i & L.documentation ?~ InR (MarkupContent MarkupKind_Markdown (T.intercalate "\n\n" header)))

-- | The modules a document can import.
modules :: Uri -> T.Text -> ServerM [(T.Text, FilePath)]
modules uri docText = case uriToFilePath uri of
  Nothing -> pure []
  Just file -> do
    State {libraryConfig, moduleLists} <- asks id
    dirs <- liftIO (Libraries.includes libraryConfig file (Source.moduleName (Source.moduleOf (T.lines docText))))
    -- libraries don't change while we run, but the document's own project may
    let project = Libraries.rootOf file (Source.moduleName (Source.moduleOf (T.lines docText)))
    liftIO . fmap concat . for dirs $ \d ->
      if project `isPrefixOf` d || d `isPrefixOf` project
        then Libraries.modulesIn d
        else modifyMVar moduleLists $ \cache -> case Map.lookup d cache of
          Just ms -> pure (cache, ms)
          Nothing -> (\ms -> (Map.insert d ms cache, ms)) <$> Libraries.modulesIn d
  where
    for = flip traverse

-- | The names a module exports, with their types. Agda says what they are:
-- it checks a scratch module that imports it (in the mirror, at the
-- document's root, so it sees the same libraries), and lists its contents.
exports :: Uri -> T.Text -> T.Text -> ServerM [(T.Text, T.Text)]
exports uri docText m = do
  State {agda, shadow, exported, scratch} <- asks id
  cached <- Map.lookup m <$> liftIO (readMVar exported)
  case (cached, uriToFilePath uri) of
    (Just names, _) -> pure names
    (Nothing, Nothing) -> pure []
    (Nothing, Just file) -> liftIO . withMVar scratch $ \() -> do
      let root = Libraries.rootOf file (Source.moduleName (Source.moduleOf (T.lines docText)))
          scratchFile = root </> "AgdaliciousCompletion.agda"
          copy = Shadow.path shadow scratchFile
      Shadow.write shadow scratchFile ("module AgdaliciousCompletion where\nimport " <> m <> "\n")
      _ <- Agda.command agda copy ("Cmd_load " <> show copy <> " []")
      rs <- Agda.command agda copy ("Cmd_show_module_contents_toplevel Simplified " <> show (T.unpack m))
      Shadow.remove shadow scratchFile
      let names =
            nub $
              [ (n, t)
              | e <- rs ^.. traverse . key "info" . key "contents" . values
              , Just n <- [e ^? key "name" . _String]
              , Just t <- [e ^? key "term" . _String]
              ]
                <> [(n, "") | n <- rs ^.. traverse . key "info" . key "names" . values . _String]
      unless (null names) $ modifyMVar_ exported (pure . Map.insert m names)
      pure names

-- | The names a document's opened imports bring into scope, and those it
-- defines. Only what is known already is offered, so that typing never waits
-- for Agda; the imported modules not asked about yet are, in the background.
inScope :: Uri -> Rope -> ServerM [(T.Text, T.Text)]
inScope uri text = do
  State {exported, documents} <- asks id
  known <- liftIO (readMVar exported)
  let docText = Rope.toText text
      is = Completion.imports docText
      missing = nub [Completion.module_ i | i <- is, not (Map.member (Completion.module_ i) known)]
  unless (null missing) $ withRunInIO $ \inIO ->
    void (async (inIO (for_ missing (exports uri docText))))
  doc <- Map.lookup uri <$> liftIO (readMVar documents)
  let typeIn names n = maybe "" id (lookup n names)
      imported =
        [ (n, typeIn names n)
        | i <- is
        , Just names <- [Map.lookup (Completion.module_ i) known]
        , n <- Completion.visible i (map fst names)
        ]
      file = uriToFilePath uri
      -- the names at their own definition sites here, other than variables
      defined =
        [ (slice (from t) (to t) text, "")
        | Just d <- [doc]
        , t <- Doc.tokens d
        , site t == ((,from t) <$> file)
        , "bound" `notElem` atoms t
        ]
  pure (nub (imported <> defined))

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
