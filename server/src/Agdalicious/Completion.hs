-- | What is being completed, from the text before the cursor, and what a
-- document's imports bring into scope.
module Agdalicious.Completion
  ( Context (..)
  , context
  , statementBefore
  , Import (..)
  , imports
  , moduleNamed
  , visible
  ) where

import Data.Char (isSpace)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T

data Context
  = Pragma Text
  -- ^ a pragma's name: @{-# BUI@
  | PragmaOption Text
  -- ^ an option in an @OPTIONS@ pragma: @{-# OPTIONS --sa@
  | Builtin Text
  -- ^ a built-in in a @BUILTIN@ pragma: @{-# BUILTIN NAT@
  | ModulePath Text
  -- ^ a module after @import@: @open import Agda.Bui@
  | ImportedName Text Text
  -- ^ a name a module exports, in a @using@, @hiding@ or @renaming@ list:
  -- @open import Agda.Builtin.IO using (I@ (the module, the name so far)
  | Code Text
  -- ^ anything else: a keyword or a name in scope
  | NoCompletion
  -- ^ something nothing is offered for, like a pragma's contents

-- | The context, and with it the text so far, which completing replaces, from
-- the statement before the cursor (see 'statementBefore').
context :: Text -> Context
context before
  | Just inside <- pragma = case (T.words inside, trailingSpace) of
      ([], _) -> Pragma ""
      ([w], False) -> Pragma w
      ("OPTIONS" : _, _) | isOption -> PragmaOption current
      ("OPTIONS" : _, True) -> PragmaOption ""
      (["BUILTIN"], True) -> Builtin ""
      (["BUILTIN", w], False) -> Builtin w
      _ -> NoCompletion
  | Just (m, rest) <- importOf, Just open <- lastList rest = ImportedName m (T.takeWhileEnd (\c -> c /= '(' && c /= ';' && not (isSpace c)) open)
  | ws <- T.words before, Just p <- path ws = ModulePath p
  | otherwise = Code current
  where
    trailingSpace = maybe True (isSpace . snd) (T.unsnoc before)
    current = T.takeWhileEnd (\c -> not (isSpace c || c `elem` ("(){};@\"" :: String))) before
    isOption = "-" `T.isPrefixOf` current
    -- the inside of a pragma that is still open
    pragma = case T.breakOnAll "{-#" before of
      [] -> Nothing
      bs -> let inside = T.drop 3 (snd (last bs)) in if "#-}" `T.isInfixOf` inside then Nothing else Just inside
    path ws = case (ws, trailingSpace) of
      (["import"], True) -> Just ""
      (["open", "import"], True) -> Just ""
      (["import", p], False) -> Just p
      (["open", "import", p], False) -> Just p
      _ -> Nothing
    importOf = case T.words before of
      "open" : "import" : m : _ -> Just (m, T.drop 1 (snd (T.breakOn m before)))
      "import" : m : _ -> Just (m, T.drop 1 (snd (T.breakOn m before)))
      "open" : m : _ -> Just (m, T.drop 1 (snd (T.breakOn m before)))
      _ -> Nothing
    -- the contents of a using/hiding/renaming list that is still open
    lastList rest = case [snd (T.breakOnEnd "(" (snd x)) | x <- T.breakOnAll "(" rest] of
      [] -> Nothing
      xs -> let l = last xs in if ")" `T.isInfixOf` l then Nothing else Just l

-- | The text of the statement the cursor is in, up to the cursor: the line
-- before it (the last of the given lines), joined to the lines above it if
-- it continues an @open@ or @import@ statement, which goes on as long as
-- its lines are indented further than its first one.
statementBefore :: [Text] -> Text
statementBefore [] = ""
statementBefore ls = case break isHead (reverse (filter (not . blank) above)) of
  (between, h : _) | all ((> indentation h) . indentation) (cursor : between) ->
    -- the line the cursor is on keeps its end (a space there matters)
    T.unwords (map T.strip (h : reverse between) <> [T.stripStart cursor])
  _ -> cursor
  where
    (above, cursor) = (init ls, last ls)

-- | An import (@open import@, @import@) or an @open@ of an imported module:
-- the module, and what it brings into scope.
data Import = Import
  { module_ :: Text
  , qualifier :: Text
  -- ^ what its names can be qualified with: the module, or the name after @as@
  , opened :: Bool
  -- ^ whether it brings its names into scope unqualified too
  , using :: Maybe [Text]
  , hiding :: [Text]
  , renaming :: [(Text, Text)]
  }

-- | A document's imports, and its @open@s of the modules it imports
-- (directly or by the name after @as@). An @open@ of any other module, such
-- as one defined in the document, is left out.
imports :: Text -> [Import]
imports t = direct <> mapMaybe opening stmts
  where
    stmts = statements (T.lines t)
    direct = mapMaybe statement stmts
    statement l = case T.words l of
      "open" : "import" : m : rest -> Just (directives m True rest l)
      "import" : m : rest -> Just (directives m False rest l)
      _ -> Nothing
    opening l = case T.words l of
      "open" : q : rest | q /= "import", Just m <- moduleNamed direct q -> Just (directives m True rest l) {qualifier = q}
      _ -> Nothing
    directives m open rest l =
      Import
        { module_ = m
        , qualifier = case dropWhile (/= "as") rest of
            _ : n : _ -> n
            _ -> m
        , opened = open
        , using = list "using" (after m l)
        , hiding = maybe [] id (list "hiding" (after m l))
        , renaming = [(a, b) | r <- maybe [] id (list "renaming" (after m l)), [a, "to", b] <- [T.words r]]
        }
    after m l = T.drop (T.length m) (snd (T.breakOn m l))
    list kw rest = case T.breakOn (kw <> " (") rest of
      (_, "") -> case T.breakOn (kw <> "(") rest of
        (_, "") -> Nothing
        (_, r) -> Just (items (T.drop (T.length kw + 1) r))
      (_, r) -> Just (items (T.drop (T.length kw + 2) r))
    items = filter (not . T.null) . map T.strip . T.splitOn ";" . fst . T.breakOn ")"

-- | The module a name in an @open@ (or a @using@ list after it) stands for:
-- an imported module, by its name or the name after @as@.
moduleNamed :: [Import] -> Text -> Maybe Text
moduleNamed is q = case [module_ i | i <- is, qualifier i == q || module_ i == q] of
  m : _ -> Just m
  [] -> Nothing

-- | A document's statements: each line that starts an @open@ or @import@,
-- with the lines after it that are indented further, joined; other lines
-- as they are.
statements :: [Text] -> [Text]
statements [] = []
statements (l : ls)
  | isHead l, (more, rest) <- span (\x -> blank x || indentation x > indentation l) ls =
      T.unwords (map T.strip (l : filter (not . blank) more)) : statements rest
  | otherwise = l : statements ls

isHead :: Text -> Bool
isHead l = case T.words l of
  "open" : _ -> True
  "import" : _ -> True
  _ -> False

blank :: Text -> Bool
blank = T.all isSpace

indentation :: Text -> Int
indentation = T.length . T.takeWhile (== ' ')

-- | The names an import brings into scope, of those its module exports.
visible :: Import -> [Text] -> [Text]
visible i exported =
  filter (`notElem` hiding i) (maybe exported (filter (`elem` exported)) (using i))
    <> [b | (a, b) <- renaming i, a `elem` exported]
