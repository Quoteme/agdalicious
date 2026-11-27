-- | What Agda's responses to a goal command ("Agdalicious.CodeAction.GoalCommand")
-- do to the document, by their kind, as the Emacs mode does: a hole filled
-- (@GiveAction@, @SolveAll@, @Mimer@), a clause split (@MakeCase@), or
-- something to show (@DisplayInfo@: a goal's type and context, an inferred
-- type, a normal form, an error, …).
module Agdalicious.CodeAction.GoalResult (Hole (..), Outcome (..), outcome) where

import Control.Applicative ((<|>))
import Control.Lens ((^..), (^?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key, values, _Bool, _Integral, _String)
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (catMaybes, listToMaybe, mapMaybe, maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Language.LSP.Protocol.Types (Position (..), Range (..))

-- | A hole of the document: where it is, and the text in it.
data Hole = Hole {holeRange :: Range, holeText :: Text}

-- | The edits of the document, and the messages (and whether they are errors).
data Outcome = Outcome {changes :: [(Range, Text)], notes :: [(Bool, Text)]}

instance Semigroup Outcome where
  Outcome a b <> Outcome c d = Outcome (a <> c) (b <> d)

instance Monoid Outcome where
  mempty = Outcome [] []

-- | What the responses to a command on a goal do, given the document's lines
-- and its holes (by number).
outcome :: [Text] -> (Int -> Maybe Hole) -> Int -> [Aeson.Value] -> Outcome
outcome docLines holes goal = foldMap response
  where
    response r = case r ^? key "kind" . _String of
      Just "GiveAction" | Just h <- holeOf r ->
        case (r ^? key "giveResult" . key "str" . _String, r ^? key "giveResult" . key "paren" . _Bool) of
          (Just s, _) -> fill h s
          (_, Just p) -> fill h (if p then "(" <> holeText h <> ")" else holeText h)
          _ -> mempty
      Just "MakeCase" | Just h <- holeOf r -> split h (r ^? key "variant" . _String) (r ^.. key "clauses" . values . _String)
      Just "SolveAll" ->
        mconcat
          [ fill h (parenthesised e)
          | s <- r ^.. key "solutions" . values
          , Just n <- [s ^? key "interactionPoint" . _Integral <|> s ^? key "interactionPoint" . key "id" . _Integral]
          , Just h <- [holes n]
          , Just e <- [s ^? key "expression" . _String]
          ]
      Just "Mimer" -> case (r ^? key "solution" . _String, holes goal) of
        (Just s, Just h) -> fill h (parenthesised s)
        _ -> note False "No solution found"
      Just "DisplayInfo" -> case r ^? key "info" . key "kind" . _String of
        Just "AllGoalsWarnings" -> mempty -- the reload shows these
        Just "Error" -> note True (maybe "" id (r ^? key "info" . key "error" . key "message" . _String <|> r ^? key "info" . key "message" . _String))
        Just "IntroNotFound" -> note False "Nothing to introduce: the goal is neither a function type nor a data or record type with a single fitting constructor"
        Just "IntroConstructorUnknown" -> note False ("Which constructor? Write one in the hole and refine: " <> T.intercalate ", " (r ^.. key "info" . key "constructors" . values . _String))
        _ -> maybe mempty (note False . render) (r ^? key "info")
      _ -> mempty
    holeOf r = holes =<< (r ^? key "interactionPoint" . key "id" . _Integral <|> Just goal)
    fill h s = Outcome [(holeRange h, s)] []
    note e t = Outcome [] [(e, t) | not (T.null t)]
    -- the clause the hole is in, replaced by the new ones
    split h variant clauses = case drop (fromIntegral l) docLines of
      line : _
        | variant == Just "ExtendedLambda" ->
            let (before, rest) = T.splitAt (charsOf line (fromIntegral (_character (_start (holeRange h))))) line
                start = T.length (T.dropWhileEnd (`notElem` ("{;" :: String)) before)
                afterHole = T.drop (charsOf line (fromIntegral (_character (_end (holeRange h)))) - T.length before) rest
                end = T.length line - T.length (T.dropWhile (`notElem` (";}" :: String)) afterHole)
             in Outcome [(Range (at line start) (at line end), " " <> T.intercalate " ; " clauses <> " ")] []
        | otherwise ->
            let indent = T.takeWhile (== ' ') line
             in Outcome [(Range (Position l 0) (at line (T.length line)), T.intercalate "\n" (map (indent <>) clauses))] []
      [] -> mempty
      where
        l = _line (_start (holeRange h))
        at line c = Position l (utf16 (T.take c line))
    -- the characters a number of UTF-16 code units of a line are
    charsOf line n = length (takeWhile (<= n) (scanl1 (+) (map width (T.unpack line))))
    width ch = if fromEnum ch > 0xFFFF then 2 else 1 :: Int
    utf16 = fromIntegral . sum . map width . T.unpack

-- | An expression to put in place of a hole, in parentheses unless it is a
-- single word (or already in parentheses).
parenthesised :: Text -> Text
parenthesised e
  | T.any (== ' ') (T.strip e) && not ("(" `T.isPrefixOf` e && ")" `T.isSuffixOf` e) = "(" <> e <> ")"
  | otherwise = e

-- | What a @DisplayInfo@ says, as text: a goal (@Goal:@, @Have:@, the
-- context), a type or normal form, a message, or else its fields.
render :: Aeson.Value -> Text
render info = case info ^? key "goalInfo" of
  Just g ->
    T.intercalate "\n" . catMaybes $
      [ ("Goal: " <>) <$> g ^? key "type" . _String
      , ("Have: " <>) <$> g ^? key "typeAux" . key "expr" . _String
      , g ^? key "expr" . _String
      , g ^? key "signature" . _String
      , context g
      ]
  Nothing ->
    maybe (decodeUtf8 (BL.toStrict (Aeson.encode info))) id $
      listToMaybe (mapMaybe (\k -> info ^? key k . _String) ["message", "expr", "contents"])
  where
    context g = case [n <> " : " <> b | e <- g ^.. key "entries" . values, n <- maybeToList (e ^? key "reifiedName" . _String), b <- maybeToList (e ^? key "binding" . _String)] of
      [] -> Nothing
      es -> Just (T.intercalate "\n" ("————" : es))
