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
--
-- The code in FOREIGN and COMPILE pragmas is the business of its backend's
-- language server (haskell-language-server for GHC, ...): it gets that code
-- as a file of its own ("Agdalicious.Foreign", "Agdalicious.Client"), and
-- hovering, going to definitions and completing there is passed on to it.
module Agdalicious.Server (run) where

import Agdalicious.Agda (Agda)
import Agdalicious.Agda qualified as Agda
import Agdalicious.Client (Client)
import Agdalicious.Client qualified as Client
import Agdalicious.CodeAction qualified as CodeAction
import Agdalicious.Completion (Context (..))
import Agdalicious.Completion qualified as Completion
import Agdalicious.Document (Document, Goal (..), Token (..), goalAt, offsetOf, positionOf, rangeOf, slice, tokenAt)
import Agdalicious.Document qualified as Doc
import Agdalicious.Foreign qualified as Foreign
import Agdalicious.Hover (Info (..), render)
import Agdalicious.Manual qualified as Manual
import Agdalicious.Source qualified as Source
import Agdalicious.Infoview (InfoviewParams (..), InfoviewResult (..))
import Agdalicious.Libraries qualified as Libraries
import Agdalicious.WorkspaceAction qualified as WorkspaceAction
import Agdalicious.Shadow (Shadow)
import Agdalicious.Shadow qualified as Shadow
import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.MVar
import Control.Lens ((%~), (&), (.~), (?~), (^.), (^..), (^?))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (withRunInIO)
import Control.Monad (filterM, guard, unless, void, when)
import Control.Monad.Reader (ReaderT, ask, asks, runReaderT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), hoistMaybe, runMaybeT)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (parseEither)
import Data.Aeson.Lens (key, values, _String)
import Data.Bifunctor (first)
import Data.Char (isAlphaNum, isAsciiLower)
import Data.Foldable (for_)
import System.FilePath (takeBaseName, takeDirectory, (<.>), (</>))
import Data.List (isPrefixOf, nub, sortOn)
import Data.Traversable (for)
import Control.Exception (IOException, try)
import Data.Maybe (catMaybes, isNothing, listToMaybe, mapMaybe, maybeToList)
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
  agdaProcess <- Agda.start
  -- the goal commands this Agda has
  known <- filterM (Agda.knows agdaProcess . (\c -> CodeAction.commandLine c 0 "")) CodeAction.goalCommands
  st <-
    State agdaProcess
      <$> pure shadow
      <*> newMVar mempty
      <*> newMVar mempty
      <*> Libraries.config
      <*> newMVar mempty
      <*> newMVar mempty
      <*> newMVar ()
      <*> newMVar mempty
      <*> newMVar mempty
      <*> newMVar mempty
      <*> pure known
  runServer
    ServerDefinition
      { parseConfig = parseSettings
      , onConfigChange = const (pure ())
      , defaultConfig = Config Foreign.defaultBackends
      , configSection = "agdalicious"
      , doInitialize = \env _req -> pure (Right env)
      , staticHandlers = const handlers
      , interpretHandler = \env -> Iso (runLspT env . flip runReaderT st) liftIO
      , options =
          defaultOptions
            { optTextDocumentSync = Just syncOptions
            , optCompletionTriggerCharacters = Just ".-"
            , optExecuteCommandCommands = Just [goalCommandName, WorkspaceAction.computeCommandName]
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
  , servers :: MVar (Map.Map T.Text (Maybe Client))
  -- ^ the language server of each backend, once needed ('Nothing' if it cannot be started)
  , synced :: MVar (Map.Map (Uri, T.Text) Synced)
  -- ^ each document's foreign code for each backend
  , agdaDiagnostics :: MVar (Map.Map Uri [Diagnostic])
  -- ^ what each document's last load said about it
  , goalCommands :: [CodeAction.GoalCommand]
  -- ^ what the Agda in use can do with a goal
  }

-- | A document's foreign code for one backend, as its language server has it.
data Synced = Synced
  { backendName :: T.Text
  , server :: Maybe Client
  , virtualFile :: FilePath
  -- ^ next to the document (in the mirror: 'virtualUri')
  , virtualUri :: Uri
  , version :: Int
  , contents :: Foreign.Virtual
  , reports :: [Diagnostic]
  -- ^ the server's diagnostics for it, in the virtual file
  }

-- | The settings: the language server for each backend's foreign code. The
-- editor can change them (in @initializationOptions.agdalicious@), as in
-- @{"foreign": {"GHC": {"command": ["hls", "--lsp"]}, "JS": null}}@; a
-- backend's @languageId@ and file @extension@ are its name in lower case
-- unless given.
newtype Config = Config {backends :: Map.Map T.Text Foreign.Backend}

parseSettings :: Config -> Aeson.Value -> Either T.Text Config
parseSettings c Aeson.Null = Right c
parseSettings _ v = first T.pack . flip parseEither v $ Aeson.withObject "settings" $ \o -> do
  given <- o Aeson..:? "foreign" Aeson..!= mempty
  pure (Config (Map.mapWithKey complete (Map.mapMaybe id (given <> fmap Just Foreign.defaultBackends))))
  where
    complete b be =
      be
        { Foreign.languageId = if T.null (Foreign.languageId be) then T.toLower b else Foreign.languageId be
        , Foreign.extension = if null (Foreign.extension be) then T.unpack (T.toLower b) else Foreign.extension be
        }

-- | The monad the handlers run in: the LSP monad, with the shared state.
type ServerM = ReaderT State (LspM Config)

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
        closeForeign uri
        reloadOpen uri
    , notificationHandler SMethod_SetTrace $ \_ ->
        pure ()
    , requestHandler SMethod_TextDocumentDefinition $ \req respond -> do
        let p = req ^. L.params
        respond . Right =<< orForeign "textDocument/definition" p (InR (InR Null)) definitionBack
          (maybe (InR (InR Null)) (InL . Definition . InL) <$> definition (p ^. L.textDocument . L.uri) (p ^. L.position))
    , requestHandler SMethod_TextDocumentHover $ \req respond -> do
        let p = req ^. L.params
        respond . Right =<< orForeign "textDocument/hover" p (InR Null) hoverBack
          (maybe (InR Null) InL <$> hover (p ^. L.textDocument . L.uri) (p ^. L.position))
    , requestHandler SMethod_TextDocumentCompletion $ \req respond -> do
        let p = req ^. L.params
        respond . Right =<< orForeign "textDocument/completion" p (InR (InR Null)) completionBack
          (InL <$> completion (p ^. L.textDocument . L.uri) (p ^. L.position))
    , requestHandler SMethod_TextDocumentCodeAction $ \req respond -> do
        let p = req ^. L.params
        respond . Right . InL =<< codeActions (p ^. L.textDocument . L.uri) (p ^. L.range) (p ^. L.context . L.diagnostics)
    , requestHandler SMethod_WorkspaceExecuteCommand $ \req respond -> do
        let p = req ^. L.params
        case (p ^. L.command, traverse Aeson.fromJSON (maybe [] id (p ^. L.arguments))) of
          (c, Aeson.Success (Aeson.String u : Aeson.Number g : Aeson.String cmd : rest)) | c == goalCommandName -> do
            -- the editor may have asked for the input (the hole being empty)
            runGoalCommand (Uri u) (round g) cmd (case drop 1 rest of Aeson.String i : _ -> Just i; _ -> Nothing)
            respond (Right (InR Null))
          (c, Aeson.Success [Aeson.String u, Aeson.String mode, Aeson.String expr]) | c == WorkspaceAction.computeCommandName -> do
            runCompute (Uri u) mode expr
            respond (Right (InR Null))
          _ -> respond (Left (TResponseError (InR ErrorCodes_InvalidParams) "unknown command" Nothing))
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
  for_ file (syncForeign uri . virtualFileText)
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
  State {agda, shadow, documents, agdaDiagnostics} <- asks id
  let copy = Shadow.path shadow file
  responses <- liftIO (Agda.command agda copy ("Cmd_load " <> show copy <> " []"))
  -- a load that fails early has no highlighting: keep what the previous one said then
  let doc = Doc.fromResponses responses
      unshadow t = t {site = first (T.unpack . Shadow.unshadow shadow . T.pack) <$> site t}
  unless (null (Doc.tokens doc)) . liftIO $
    modifyMVar_ documents (pure . Map.insert uri doc {Doc.tokens = map unshadow (Doc.tokens doc)})
  liftIO . modifyMVar_ agdaDiagnostics . (pure .) . Map.insert uri $
    map (diagnostic file . fmap (Shadow.unshadow shadow)) (concatMap messagesOf responses)
  publish uri

-- | Publish a document's diagnostics: Agda's, and those the language servers
-- of its foreign code have.
publish :: Uri -> ServerM ()
publish uri = do
  State {agdaDiagnostics, synced} <- asks id
  fromAgda <- Map.findWithDefault [] uri <$> liftIO (readMVar agdaDiagnostics)
  ss <- liftIO (readMVar synced)
  let fromServers =
        [ d & L.range .~ r & L.relatedInformation .~ Nothing
        | ((u, _), s) <- Map.toList ss
        , u == uri
        , d <- reports s
        , Just r <- [Foreign.reported (contents s) (d ^. L.range)]
        ]
  sendNotification SMethod_TextDocumentPublishDiagnostics (PublishDiagnosticsParams uri Nothing (fromAgda <> fromServers))

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

-- * Code actions

-- | The fixes for Agda's diagnostics ("Agdalicious.CodeAction"): for a name
-- not in scope, importing (or writing) what Agda suggests, or, if it has
-- nothing to import, importing a module that defines the name. The only
-- fix for a diagnostic is marked preferred, so that the editor can apply it
-- without listing it. And, anywhere, writing @?@-holes as @{! !}@.
codeActions :: Uri -> Range -> [Diagnostic] -> ServerM [Command |? CodeAction]
codeActions uri cursor ds = do
  rope <- maybe mempty (^. file_text) <$> getVirtualFile (toNormalizedUri uri)
  loaded <- Map.lookup uri <$> (liftIO . readMVar =<< asks documents)
  let doc = Rope.toText rope
      -- the holes of the last load that are still a ?
      questions =
        [ rangeOf (goalFrom g) (goalTo g) rope
        | g <- maybe [] Doc.goals loaded
        , slice (goalFrom g) (goalTo g) rope == "?"
        ]
      holeActions =
        [InR (action CodeActionKind_RefactorRewrite Nothing False f) | f <- CodeAction.questionToMeta questions cursor]
  commands <- asks goalCommands
  -- what Agda can do with the hole the cursor is in
  let goalActions =
        [ InR (CodeAction (CodeAction.menuTitle c) (Just CodeActionKind_RefactorRewrite) Nothing Nothing Nothing Nothing (Just command) Nothing)
        | Just off <- [offsetOf (cursor ^. L.start) rope]
        , Just g <- [goalAt off =<< fmap Doc.goals loaded]
        , c <- commands
        , let command = Command (CodeAction.menuTitle c) goalCommandName (Just [Aeson.toJSON uri, Aeson.toJSON (goal g), Aeson.toJSON (CodeAction.agdaCommand c), Aeson.toJSON (CodeAction.prompt c), Aeson.toJSON (holeContents (slice (goalFrom g) (goalTo g) rope))])
        ]
      -- evaluating what is selected, outside a hole (in one, the goal's do)
      computeActions =
        [ InR (CodeAction (WorkspaceAction.computeTitle c <> " (selection)") Nothing Nothing Nothing Nothing Nothing (Just command) Nothing)
        | null goalActions
        , Just from <- [offsetOf (cursor ^. L.start) rope]
        , Just to <- [offsetOf (cursor ^. L.end) rope]
        , let expr = T.strip (slice from to rope)
        , not (T.null expr)
        , c <- WorkspaceAction.computes
        , let command = Command (WorkspaceAction.computeTitle c) WorkspaceAction.computeCommandName (Just [Aeson.toJSON uri, Aeson.toJSON (WorkspaceAction.computeMode c), Aeson.toJSON expr])
        ]
  fmap ((<> (goalActions <> computeActions <> holeActions)) . concat) . for [d | d <- ds, d ^. L.source == Just "agda"] $ \d -> do
    let suggested = CodeAction.notInScope doc (d ^. L.message) (d ^. L.range)
        imports = filter (("Import " `T.isPrefixOf`) . CodeAction.title) suggested
    found <- case CodeAction.missingName (d ^. L.message) of
      Just n | null imports -> importsOf uri doc n
      _ -> pure []
    let fs = found <> suggested
    pure [InR (action CodeActionKind_QuickFix (Just [d]) (length fs == 1) f) | f <- fs]
  where
    action kind about preferred f =
      CodeAction
        (CodeAction.title f)
        (Just kind)
        about
        (Just preferred)
        Nothing -- disabled
        (Just (WorkspaceEdit (Just (Map.singleton uri [TextEdit r t | (r, t) <- CodeAction.edits f])) Nothing Nothing))
        Nothing -- command
        Nothing -- data

-- | Importing a name from one of the modules the document can import but
-- does not: those whose sources seem to define it, or re-export all of a
-- module that does (as @Data.Nat@ does @Data.Nat.Base@), and whose exports,
-- as Agda lists them, have it. Modules with shorter names come first (the
-- built-in ones and the main modules of a library), and only so many are
-- asked about, as Agda takes a while for each the first time.
importsOf :: Uri -> T.Text -> T.Text -> ServerM [CodeAction.Fix]
importsOf uri doc n = do
  ms <- modules uri doc
  let own = maybeToList (Source.moduleName (Source.moduleOf (T.lines doc)))
      imported = map Completion.module_ (Completion.imports doc) <> own
  sources <- liftIO . for ms $ \(m, f) -> do
    src <- try (T.readFile f) :: IO (Either IOException T.Text)
    pure [(m, (CodeAction.definitions n t, CodeAction.reexports t)) | Right t <- [src]]
  let scanned = concat sources
      direct = [(m, d) | (m, (ds, _)) <- scanned, d <- ds]
      -- three levels of re-exports are plenty
      through known = nub (known <> [(m, d) | (m, (_, rs)) <- scanned, (r, d) <- known, r `elem` rs])
      found = iterate through direct !! 3
      candidates = take 10 (sortOn (T.length . fst) [c | c@(m, _) <- found, m `notElem` imported])
  fmap concat . for candidates $ \(m, d) -> do
    names <- map fst <$> exports uri doc m
    pure [CodeAction.importModule doc m d | d `elem` names]

-- | The command that runs a goal command. Its arguments: the document, the
-- goal, the command, what to ask for if the hole is empty ('Nothing' if
-- nothing), and the text in the hole, which an editor that asks replaces
-- with the answer (see @nvim/lua/agdalicious/lsp.lua@).
goalCommandName :: T.Text
goalCommandName = "agdalicious.goal"

-- | Run an Agda command on a goal (with the text in its hole), and do what
-- its responses say: edit the document, or show a message.
runGoalCommand :: Uri -> Int -> T.Text -> Maybe T.Text -> ServerM ()
runGoalCommand uri g cmd input = do
  rope <- maybe mempty (^. file_text) <$> getVirtualFile (toNormalizedUri uri)
  loaded <- Map.lookup uri <$> (liftIO . readMVar =<< asks documents)
  let holes = maybe [] Doc.goals loaded
      hole n = listToMaybe [CodeAction.Hole (rangeOf (goalFrom h) (goalTo h) rope) (holeContents (slice (goalFrom h) (goalTo h) rope)) | h <- holes, goal h == n]
  let tell kind = sendNotification SMethod_WindowShowMessage . ShowMessageParams kind
  case hole g of
    Nothing -> tell MessageType_Warning "agdalicious: that hole is gone (the file changed since it was last checked): try again once it is checked"
    Just h -> do
      rs <- query uri (CodeAction.commandLine (CodeAction.GoalCommand "" cmd Nothing) g (maybe (CodeAction.holeText h) id input))
      let CodeAction.Outcome {CodeAction.changes, CodeAction.notes} = CodeAction.outcome (T.lines (Rope.toText rope)) hole g rs
      unless (null changes) . void $
        sendRequest SMethod_WorkspaceApplyEdit (ApplyWorkspaceEditParams (Just cmd) (WorkspaceEdit (Just (Map.singleton uri [TextEdit r t | (r, t) <- changes])) Nothing Nothing)) (const (pure ()))
      sh <- asks shadow
      for_ notes $ \(err, t) -> tell (if err then MessageType_Error else MessageType_Info) (Shadow.unshadow sh t)
      -- Agda says nothing when, for one, there is nothing in the hole to give
      when (null changes && null notes) . tell MessageType_Warning $
        if T.null (CodeAction.holeText h)
          then "Agda did nothing: this command works on what is in the hole. Write it there ({! … !}, see \"Expand ? to {! !}\") and try again"
          else "Agda did nothing with " <> CodeAction.holeText h

-- * Workspace actions

-- | Evaluate an expression in the scope of a document
-- ("Agdalicious.WorkspaceAction.ComputeToplevel"), and show its normal form.
runCompute :: Uri -> T.Text -> T.Text -> ServerM ()
runCompute uri mode expr = do
  rs <- query uri (WorkspaceAction.computeLine mode expr)
  sh <- asks shadow
  let tell kind = sendNotification SMethod_WindowShowMessage . ShowMessageParams kind . Shadow.unshadow sh
  case WorkspaceAction.computeResult rs of
    Right nf -> tell MessageType_Info (expr <> " ⟶ " <> nf)
    Left err -> tell MessageType_Error err

-- | The text in a hole (@{! x !}@: @x@; @?@: nothing).
holeContents :: T.Text -> T.Text
holeContents t = maybe "" T.strip (T.stripPrefix "{!" t >>= T.stripSuffix "!}")

-- * Completion

-- | What can be written at a position: keywords, pragmas, options and
-- built-ins (documented from the manual), modules, and names.
completion :: Uri -> Position -> ServerM [CompletionItem]
completion uri pos@(Position l c) = fmap (maybe [] id) . runMaybeT $ do
  text <- MaybeT (fmap (^. file_text) <$> getVirtualFile (toNormalizedUri uri))
  line <- hoistMaybe (Rope.toText . fst <$> Rope.utf16SplitAt (fromIntegral c) (Rope.getLine (fromIntegral l) text))
  -- the statement so far, which may have begun on a line above
  let lineNo = fromIntegral l :: Int -- (l is unsigned)
      above = [Rope.toText (Rope.getLine (fromIntegral k) text) | k <- [max 0 (lineNo - 30) .. lineNo - 1]]
      before = Completion.statementBefore (map (T.dropWhileEnd (== '\n')) above <> [line])
  let edit w i = i & L.textEdit ?~ InL (TextEdit (Range (Position l (c - utf16Length w)) pos) (i ^. L.label))
      documented kind w = [edit w (manualItem k e kind) | (k, e) <- Manual.termsOf kind, kind /= "keyword" || T.any isAsciiLower k]
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
    ImportedName q w -> do
      let m = maybe q id (Completion.moduleNamed (Completion.imports docText) q)
      names <- exports uri docText m
      pure [edit w (nameItem n t) | (n, t) <- names]
    Code w -> do
      -- qualified names only once a qualifier is being written
      names <- inScope uri text ("." `T.isInfixOf` w)
      pure (documented "keyword" w <> [edit w (nameItem n t) | (n, t) <- names])
    NoCompletion -> pure []
  where
    utf16Length = fromIntegral . T.foldl' (\n ch -> n + if fromEnum ch > 0xFFFF then 2 else 1) (0 :: Int)

item :: T.Text -> CompletionItemKind -> CompletionItem
item label kind =
  CompletionItem label Nothing (Just kind) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | A keyword, pragma, option or built-in, with what the manual says about it.
-- (Of the keywords, only words are offered: symbols aren't worth completing.)
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
resolveItem i = case (i ^? L.data_ . traverse . key "file" . _String, foreignData) of
  (_, Just (b, uri, original)) -> do
    s <- Map.lookup (uri, b) <$> (liftIO . readMVar =<< asks synced)
    resolved <- maybe (pure Nothing) (\s' -> askServer s' "completionItem/resolve" original) s
    pure (maybe i (\s' -> maybe i (foreignItem uri b (contents s')) resolved) s)
  (Nothing, _) -> pure i
  (Just f, _) -> do
    src <- textOf (T.unpack f)
    let header = Source.header (Source.moduleOf (Rope.lines src))
    pure (if null header then i else i & L.documentation ?~ InR (MarkupContent MarkupKind_Markdown (T.intercalate "\n\n" header)))
  where
    foreignData = do
      d <- i ^. L.data_
      (,,) <$> d ^? key "foreign" . _String <*> (Uri <$> d ^? key "uri" . _String) <*> d ^? key "item"

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

-- | The names a document's imports and @open@s bring into scope (qualified
-- ones too, if asked for), and those it defines. Only what is known already
-- is offered, so that typing never waits for Agda; the imported modules not
-- asked about yet are, in the background.
inScope :: Uri -> Rope -> Bool -> ServerM [(T.Text, T.Text)]
inScope uri text qualified = do
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
        [ (q <> n, typeIn names n)
        | i <- is
        , Just names <- [Map.lookup (Completion.module_ i) known]
        , n <- Completion.visible i (map fst names)
        , q <- ["" | Completion.opened i] <> [Completion.qualifier i <> "." | qualified]
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

-- * Foreign code

-- | Hand a document's foreign code to the language servers of its backends,
-- and keep where it is for 'foreignAt'.
syncForeign :: Uri -> T.Text -> ServerM ()
syncForeign uri doc = for_ (uriToFilePath uri) $ \file -> do
  State {shadow, synced} <- ask
  Config {backends} <- getConfig
  let ps = Foreign.pragmas doc
      -- the module name a Haskell file of it must have, and its name
      name = "Foreign_" <> T.map (\c -> if isAlphaNum c then c else '_') (T.pack (takeBaseName file))
  before <- (\ss -> [b | (u, b) <- Map.keys ss, u == uri]) <$> liftIO (readMVar synced)
  -- a backend whose code is gone gets an empty file
  for_ (nub (map Foreign.backend ps <> before)) $ \b -> do
    let be = Map.lookup b backends
        v = Foreign.virtual (maybe "" Foreign.languageId be) name b ps
        vfile = takeDirectory file </> T.unpack name <.> maybe "" Foreign.extension be
        vuri = filePathToUri (Shadow.path shadow vfile)
        textDocument ver = ["uri" .= vuri, "version" .= ver]
    srv <- maybe (pure Nothing) (serverFor b (takeDirectory file)) be
    liftIO $ do
      Shadow.write shadow vfile (Foreign.text v)
      modifyMVar_ synced $ \m -> do
        let old = Map.lookup (uri, b) m
            changed = maybe True ((/= Foreign.text v) . Foreign.text . contents) old
            ver = maybe 1 ((+ fromEnum changed) . version) old
        for_ srv $ \c -> case old of
          Nothing ->
            Client.notify c "textDocument/didOpen" $
              Aeson.object ["textDocument" .= Aeson.object (textDocument ver <> ["languageId" .= maybe "" Foreign.languageId be, "text" .= Foreign.text v])]
          Just _ | changed ->
            Client.notify c "textDocument/didChange" $
              Aeson.object ["textDocument" .= Aeson.object (textDocument ver), "contentChanges" .= [Aeson.object ["text" .= Foreign.text v]]]
          _ -> pure ()
        pure (Map.insert (uri, b) (Synced b srv vfile vuri ver v (maybe [] reports old)) m)
  publish uri

-- | A document was closed: so is its foreign code.
closeForeign :: Uri -> ServerM ()
closeForeign uri = do
  State {shadow, synced} <- ask
  liftIO $ modifyMVar_ synced $ \m -> do
    let (gone, kept) = Map.partitionWithKey (\(u, _) _ -> u == uri) m
    for_ gone $ \s -> do
      for_ (server s) $ \c -> Client.notify c "textDocument/didClose" (Aeson.object ["textDocument" .= Aeson.object ["uri" .= virtualUri s]])
      Shadow.remove shadow (virtualFile s)
    pure kept

-- | The language server of a backend, started the first time it is needed
-- (in the mirror's copy of the workspace, or of @dir@ if there is none).
serverFor :: T.Text -> FilePath -> Foreign.Backend -> ServerM (Maybe Client)
serverFor b dir be = do
  State {servers, shadow} <- ask
  known <- Map.lookup b <$> liftIO (readMVar servers)
  case known of
    Just c -> pure c
    Nothing -> do
      caps <- getClientCapabilities
      root <- Shadow.path shadow . maybe dir id <$> getRootPath
      let params =
            Aeson.object
              [ "processId" .= Null
              , "clientInfo" .= Aeson.object ["name" .= ("agdalicious-lsp" :: T.Text)]
              , "rootUri" .= filePathToUri root
              , "capabilities" .= caps
              ]
      let cmd = T.unwords (map T.pack (Foreign.command be))
          stopped logLines =
            sendNotification SMethod_WindowShowMessage . ShowMessageParams MessageType_Error $
              T.unlines (("agdalicious-lsp: " <> cmd <> " (for " <> b <> " code) stopped:") : filter (not . T.null . T.strip) logLines)
      c <- withRunInIO $ \inIO -> Client.start (Foreign.command be) root params (\m p -> inIO (fromServer b m p)) (inIO . stopped)
      when (isNothing c) . logInfo $ "agdalicious-lsp: cannot start " <> T.unwords (map T.pack (Foreign.command be)) <> " for " <> b <> " code"
      liftIO (modifyMVar_ servers (pure . Map.insert b c))
      pure c

-- | A notification from a backend's language server: its diagnostics are
-- the documents', its messages (such as why it cannot work) the editor's.
fromServer :: T.Text -> T.Text -> Aeson.Value -> ServerM ()
fromServer b m params
  | m == "window/showMessage", Aeson.Success (ShowMessageParams t msg) <- Aeson.fromJSON params =
      sendNotification SMethod_WindowShowMessage (ShowMessageParams t (prefixed msg))
  | m == "window/logMessage", Aeson.Success (LogMessageParams t msg) <- Aeson.fromJSON params =
      sendNotification SMethod_WindowLogMessage (LogMessageParams t (prefixed msg))
  | otherwise = fromServerDiagnostics b m params
  where
    prefixed msg = "agdalicious-lsp (" <> b <> " code): " <> msg

fromServerDiagnostics :: T.Text -> T.Text -> Aeson.Value -> ServerM ()
fromServerDiagnostics b m params = case Aeson.fromJSON params of
  Aeson.Success (PublishDiagnosticsParams vuri v ds) | m == "textDocument/publishDiagnostics" -> do
    State {synced} <- ask
    hit <- liftIO . modifyMVar synced $ \ss ->
      pure $ case [k | (k@(_, b'), s) <- Map.toList ss, b' == b, toNormalizedUri (virtualUri s) == toNormalizedUri vuri, maybe True ((== version s) . fromIntegral) v] of
        k : _ -> (Map.adjust (\s -> s {reports = ds}) k ss, Just (fst k))
        [] -> (ss, Nothing)
    for_ hit publish
  _ -> pure ()

-- | The foreign code at a position of a document, if it is in some: what
-- its backend's language server has of it, and the position there.
foreignAt :: Uri -> Position -> ServerM (Maybe (Synced, Position))
foreignAt uri pos = do
  ss <- liftIO . readMVar =<< asks synced
  pure (listToMaybe [(s, p) | ((u, _), s) <- Map.toList ss, u == uri, Just p <- [Foreign.toVirtual (contents s) pos]])

-- | Answer a request about a position: in foreign code, its backend's
-- language server does (or nobody: @none@), and its answer is mapped back
-- into the document with @back@; elsewhere @agda@ does.
orForeign ::
  (Aeson.ToJSON params, Aeson.FromJSON result, L.HasTextDocument params TextDocumentIdentifier, L.HasPosition params Position) =>
  T.Text -> params -> result -> (Uri -> Synced -> result -> ServerM result) -> ServerM result -> ServerM result
orForeign method p none back agda = do
  let uri = p ^. L.textDocument . L.uri
  foreignAt uri (p ^. L.position) >>= \case
    Nothing -> agda
    Just (s, vp) ->
      askServer s method (p & L.textDocument . L.uri .~ virtualUri s & L.position .~ vp)
        >>= maybe (pure none) (back uri s)

-- | Send a request to the language server of some foreign code.
askServer :: (Aeson.ToJSON params, Aeson.FromJSON result) => Synced -> T.Text -> params -> ServerM (Maybe result)
askServer s method p = case server s of
  Nothing -> pure Nothing
  Just c -> do
    r <- liftIO (Client.request c method (Aeson.toJSON p))
    pure $ case Aeson.fromJSON <$> r of
      Right (Aeson.Success a) -> Just a
      _ -> Nothing

hoverBack :: Uri -> Synced -> Hover |? Null -> ServerM (Hover |? Null)
hoverBack _ s = pure . \case
  InL h | not (empty (h ^. L.contents)) -> InL (h & L.range %~ (>>= Foreign.fromVirtualRange (contents s)))
  _ -> InR Null
  where
    -- as haskell-language-server's hover of a keyword is
    empty = \case
      InL (MarkupContent _ t) -> T.null (T.strip t)
      _ -> False

-- | Locations in the virtual file are in the document; those in the mirror
-- are in the original files.
definitionBack :: Uri -> Synced -> Definition |? ([DefinitionLink] |? Null) -> ServerM (Definition |? ([DefinitionLink] |? Null))
definitionBack uri s r = do
  sh <- asks shadow
  let at' u r'
        | toNormalizedUri u == toNormalizedUri (virtualUri s) = (uri,) <$> Foreign.fromVirtualRange (contents s) r'
        | otherwise = Just (Uri (Shadow.unshadow sh (getUri u)), r')
      location (Location u r') = uncurry Location <$> at' u r'
      link (DefinitionLink (LocationLink origin u target selection)) = do
        (u', target') <- at' u target
        (_, selection') <- at' u selection
        pure (DefinitionLink (LocationLink (origin >>= Foreign.fromVirtualRange (contents s)) u' target' selection'))
  pure $ case r of
    InL (Definition (InL l)) -> maybe (InR (InR Null)) (InL . Definition . InL) (location l)
    InL (Definition (InR ls)) -> InL (Definition (InR (mapMaybe location ls)))
    InR (InL ls) -> InR (InL (mapMaybe link ls))
    _ -> r

completionBack :: Uri -> Synced -> [CompletionItem] |? (CompletionList |? Null) -> ServerM ([CompletionItem] |? (CompletionList |? Null))
completionBack uri s r = pure $ case r of
  InL is -> InL (map back is)
  InR (InL l) -> InR (InL (l & L.items %~ map back & L.itemDefaults %~ fmap (L.editRange .~ Nothing)))
  _ -> r
  where
    back = foreignItem uri (backendName s) (contents s)

-- | A completion item of a backend's language server, for the document: its
-- edits mapped back (those that cannot be are left out), and the item kept
-- in its data, for resolving it ('resolveItem').
foreignItem :: Uri -> T.Text -> Foreign.Virtual -> CompletionItem -> CompletionItem
foreignItem uri b v i =
  i
    & L.textEdit %~ (>>= edit)
    & L.additionalTextEdits %~ fmap (mapMaybe textEdit)
    & L.data_ ?~ Aeson.object ["foreign" .= b, "uri" .= uri, "item" .= i]
  where
    textEdit (TextEdit r t) = (`TextEdit` t) <$> Foreign.fromVirtualRange v r
    edit (InL e) = InL <$> textEdit e
    edit (InR (InsertReplaceEdit t ins rep)) = InR <$> (InsertReplaceEdit t <$> Foreign.fromVirtualRange v ins <*> Foreign.fromVirtualRange v rep)
