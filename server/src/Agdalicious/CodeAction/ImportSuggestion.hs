-- | Quick fixes for a name that is not in scope, from the names Agda
-- suggests instead: the @did you mean …@ of its error, which lists names in
-- scope that look alike, qualified ones of imported modules included:
--
-- > Not in scope:
-- >   +
-- >   at /x/Main.agda:34.31-32
-- >     (did you mean 'Agda.Builtin.Nat._+_'?)
--
-- A name of a module the document imports, but does not open the name of,
-- gets imported: it is added to the @using@ list of the module's @open@ (or
-- an @open … using (…)@ is added after a plain @import@). Any suggestion can
-- also be written instead of the name. Agda does not suggest names of
-- modules that are not imported.
module Agdalicious.CodeAction.ImportSuggestion (notInScope) where

import Agdalicious.CodeAction.Fix (Fix (..))
import Data.Char (isSpace)
import Data.List (sortOn)
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Language.LSP.Protocol.Types (Position (..), Range (..))

-- | The fixes for a @NotInScope@ error (its message) at a range of a document.
notInScope :: Text -> Text -> Range -> [Fix]
notInScope doc message range
  | not ("[NotInScope]" `T.isInfixOf` message) = []
  | otherwise = imports <> replacements
  where
    names = suggestions message
    stmts = statements (zip [0 ..] (T.lines doc))
    imports = mapMaybe importing names
    -- the longest qualifier that the suggestion is a name of
    importing s = listToMaybe $
      [ Fix ("Import " <> n <> " from " <> q) [e]
      | (q, n) <- sortOn (Down . T.length . fst) (splits s)
      , Just e <- [importEdit stmts q n]
      ]
    splits s = [(q, n) | st <- stmts, let q = qualifier st, Just n <- [T.stripPrefix (q <> ".") s], not ("." `T.isInfixOf` n)]
    -- an operator (@_+_@) can't be written for a part of one (@+@)
    replacements =
      [ Fix ("Write " <> s <> " instead") [(range, s)]
      | s <- sortOn (T.count ".") names -- the shortest way to write it first
      , not ("_" `T.isInfixOf` snd (T.breakOnEnd "." s))
      ]

-- | The names of the @did you mean@ list: @'A.x' or 'B.x'@.
suggestions :: Text -> [Text]
suggestions msg = case T.breakOn "did you mean" msg of
  (_, "") -> []
  (_, rest) ->
    filter (not . T.null)
      . map (unquote . T.strip)
      . T.splitOn "' or"
      . fst
      . T.breakOn "?)"
      $ T.drop (T.length "did you mean") rest
  where
    unquote t = let u = maybe t id (T.stripPrefix "'" t) in maybe u id (T.stripSuffix "'" u)

-- | An @open@ or @import@ statement, as far as an edit of it needs.
data Statement = Statement
  { qualifier :: Text
  -- ^ what its module's names are qualified with
  , opens :: Bool
  , usingList :: Maybe (Position, Bool)
  -- ^ where its @using@ list's closing parenthesis is, and whether the list is empty
  , plain :: Bool
  -- ^ an @import@ with no @open@ nor @using@, @hiding@ or @renaming@
  , indent :: Int
  , lastLine :: Int
  }

-- | The edit that brings @n@ of the module qualified with @q@ into scope.
importEdit :: [Statement] -> Text -> Text -> Maybe (Range, Text)
importEdit stmts q n = case [s | s <- ofQ, opens s, isJust (usingList s)] of
  Statement {usingList = Just (p, empty)} : _ -> Just (Range p p, if empty then n else "; " <> n)
  _ -> case [s | s <- ofQ, plain s] of
    s : _ ->
      let p = Position (fromIntegral (lastLine s + 1)) 0
       in Just (Range p p, T.replicate (indent s) " " <> "open " <> q <> " using (" <> n <> ")\n")
    [] -> Nothing
  where
    ofQ = filter ((== q) . qualifier) stmts

-- | The @open@ and @import@ statements of numbered lines: a line that starts
-- one, and the lines after it that are indented further.
statements :: [(Int, Text)] -> [Statement]
statements [] = []
statements ((i, l) : ls) = case T.words l of
  "open" : "import" : m : _ -> stmt True m
  "import" : m : _ -> stmt False m
  "open" : m : _ -> stmt True m
  _ -> statements ls
  where
    indentation = T.length . T.takeWhile (== ' ')
    (more, rest) = span (\(_, x) -> T.all isSpace x || indentation x > indentation l) ls
    body = (i, l) : more
    stmt open m =
      let ws = concatMap (T.words . snd) body
          q = case dropWhile (/= "as") ws of
            _ : a : _ -> a
            _ -> m
          directive = any (`elem` ["using", "hiding", "renaming", "public"]) ws
       in Statement
            { qualifier = q
            , opens = open
            , usingList = usingClose body
            , plain = not open && not directive
            , indent = indentation l
            , lastLine = fst (last (filter (not . T.all isSpace . snd) body))
            }
            : statements rest

-- | Where the closing parenthesis of a @using (…)@ is, and whether the list
-- is empty, in a statement's lines.
usingClose :: [(Int, Text)] -> Maybe (Position, Bool)
usingClose body = listToMaybe (mapMaybe at (zip [0 ..] cs))
  where
    cs = [(Position (fromIntegral n) (utf16 (T.take c t)), ch) | (n, t) <- body, (c, ch) <- zip [0 ..] (T.unpack t <> " ")]
    utf16 = fromIntegral . T.foldl' (\k ch -> k + if fromEnum ch > 0xFFFF then 2 else 1) (0 :: Int)
    at (k, _)
      | map snd (take 5 (drop k cs)) == "using"
      , k == 0 || isSpace (snd (cs !! (k - 1)))
      , (_, '(') : inside <- dropWhile (isSpace . snd) (drop (k + 5) cs)
      , (items, (p, ')') : _) <- break ((== ')') . snd) inside =
          Just (p, all (isSpace . snd) items)
      | otherwise = Nothing
