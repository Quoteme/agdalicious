-- | What is being completed, from the line before the cursor, and what a
-- document's imports bring into scope.
module Agdalicious.Completion (Context (..), context, Import (..), imports, visible) where

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

-- | The context, and with it the text so far, which completing replaces.
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

-- | An @open import@ (or @open@) line: the module, and the names it brings
-- into scope unqualified.
data Import = Import
  { module_ :: Text
  , using :: Maybe [Text]
  , hiding :: [Text]
  , renaming :: [(Text, Text)]
  }

-- | The imports that open their module, one per line.
imports :: Text -> [Import]
imports = mapMaybe line . T.lines
  where
    line l = case T.words l of
      "open" : "import" : m : _ -> Just (directives m (T.drop (T.length m) (snd (T.breakOn m l))))
      _ -> Nothing
    directives m rest =
      Import
        { module_ = m
        , using = list "using" rest
        , hiding = maybe [] id (list "hiding" rest)
        , renaming = [(a, b) | r <- maybe [] id (list "renaming" rest), [a, "to", b] <- [T.words r]]
        }
    list kw rest = case T.breakOn (kw <> " (") rest of
      (_, "") -> case T.breakOn (kw <> "(") rest of
        (_, "") -> Nothing
        (_, r) -> Just (items (T.drop (T.length kw + 1) r))
      (_, r) -> Just (items (T.drop (T.length kw + 2) r))
    items = filter (not . T.null) . map T.strip . T.splitOn ";" . fst . T.breakOn ")"

-- | The names an import brings into scope, of those its module exports.
visible :: Import -> [Text] -> [Text]
visible i exported =
  filter (`notElem` hiding i) (maybe exported (filter (`elem` exported)) (using i))
    <> [b | (a, b) <- renaming i, a `elem` exported]
