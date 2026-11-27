-- | Quick fixes for a name that is not in scope and that Agda has nothing to
-- suggest for, as its module is not imported (for the modules that are, see
-- "Agdalicious.CodeAction.ImportSuggestion"): importing a module that
-- defines it, with @open import M using (x)@ after the document's imports.
--
-- Which modules define a name is read off their sources ('definitions',
-- 'reexports'), roughly: the server has Agda confirm it before offering the
-- import.
module Agdalicious.CodeAction.ImportModule (missingName, definitions, reexports, importModule) where

import Agdalicious.CodeAction.Fix (Fix (..))
import Data.Char (isSpace)
import Data.List (nub)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Language.LSP.Protocol.Types (Position (..), Range (..))

-- | The name a @NotInScope@ error is about.
missingName :: Text -> Maybe Text
missingName msg
  | not ("[NotInScope]" `T.isInfixOf` msg) = Nothing
  | otherwise = case T.breakOn "Not in scope:" msg of
      (_, "") -> Nothing
      (_, rest) -> listToMaybe (T.words (T.drop (T.length "Not in scope:") rest))

-- | The names a module's source defines (or re-exports) that a missing name
-- may stand for: the name itself, or the operator it is a part of (@_+_@
-- for @+@). Only a guess: it also finds what is private, or local.
definitions :: Text -> Text -> [Text]
definitions name src
  | not (name `T.isInfixOf` src) = []
  | otherwise = nub (filter matches (concatMap (defined . T.words) (statements src)))
  where
    matches d = d == name || ("_" `T.isInfixOf` d && name `elem` T.splitOn "_" d)
    defined ws = case ws of
      "data" : n : _ -> [n]
      "record" : n : _ -> [n]
      _
        | "public" `elem` ws -> reexported ws
        | (ns@(_ : _), ":" : _) <- break (== ":") (filter (`notElem` modifiers) ws), all nameLike ns -> ns
        | otherwise -> []
    modifiers = ["field", "postulate", "private", "abstract", "instance", "macro", "primitive"]
    nameLike n = not (T.any (`elem` ("(){}" :: String)) n) && n `notElem` ["data", "record", "open", "import", "module", "where", "λ", "\\", "∀"]
    -- the names of an @open import M public using (…) renaming (… to …)@
    reexported ws = go False (concatMap (T.words . T.map (\c -> if c `elem` ("();" :: String) then ' ' else c)) ws)
      where
        go _ [] = []
        go _ ("using" : rest) = go True rest
        go _ ("to" : n : rest) = n : go False rest
        go _ (k : rest) | k `elem` ["renaming", "hiding", "public"] = go False rest
        go True (n : rest) = n : go True rest
        go False (_ : rest) = go False rest

-- | The modules a module's source re-exports all of: @open import M public@.
reexports :: Text -> [Text]
reexports src =
  [ m
  | st <- statements src
  , "open" : "import" : m : rest <- [T.words st]
  , "public" `elem` rest
  , not (any (`elem` rest) ["using", "hiding", "renaming"])
  ]

-- | A source's lines, those of a statement that goes on over the lines
-- indented further (such as an @open import@ with a @using@ list) joined.
statements :: Text -> [Text]
statements = go . T.lines
  where
    indentation = T.length . T.takeWhile (== ' ')
    go [] = []
    go (l : ls)
      | T.all isSpace l = go ls
      | otherwise =
          let (more, rest) = span (\x -> T.all isSpace x || indentation x > indentation l) ls
              -- a block (after @where@, @postulate@, …) is not one statement
              joined = if any (`elem` ["open", "import"]) (take 2 (T.words l)) then more else []
           in T.unwords (l : joined) : go (if null joined then ls else rest)

-- | Import a name of a module: @open import M using (x)@, after the
-- document's last import (or its module header, or at its start).
importModule :: Text -> Text -> Text -> Fix
importModule doc m n = Fix ("Import " <> n <> " from " <> m) [(Range p p, T.replicate indent " " <> "open import " <> m <> " using (" <> n <> ")\n")]
  where
    ls = zip [0 :: Int ..] (T.lines doc)
    indentation = T.length . T.takeWhile (== ' ')
    starts ws = case ws of
      "open" : "import" : _ -> True
      "import" : _ -> True
      _ -> False
    lastImport = [(i, l) | (i, l) <- ls, starts (T.words l)]
    header = [(i, l) | (i, l) <- ls, take 1 (T.words l) == ["module"]]
    (line, indent) = case (reverse lastImport, header) of
      ((i, l) : _, _) -> (end i l, indentation l)
      ([], (i, _) : _) -> (i + 1, 0)
      _ -> (0, 0)
    -- a statement goes on over the lines indented further
    end i l =
      let more = takeWhile (\(_, x) -> T.all isSpace x || indentation x > indentation l) (drop (i + 1) ls)
       in maybe i fst (listToMaybe (reverse (filter (not . T.all isSpace . snd) more))) + 1
    p = Position (fromIntegral line) 0
