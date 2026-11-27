-- | The code that FOREIGN and COMPILE pragmas carry for a backend, such as
-- @{-# FOREIGN GHC import Data.Maybe #-}@, as a source file of the backend's
-- language, so that a language server of that language can work on it.
--
-- Each line of that file is generated text (a module header, the binding a
-- COMPILE expression gets) followed by a piece of one line of the Agda
-- document, so positions map line by line. Positions are LSP's: lines and
-- UTF-16 code units from 0.
module Agdalicious.Foreign
  ( Backend (..)
  , defaultBackends
  , Pragma (..)
  , pragmas
  , Virtual
  , virtual
  , text
  , toVirtual
  , fromVirtual
  , fromVirtualRange
  , reported
  ) where

import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Char (isSpace)
import Data.List (partition)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Language.LSP.Protocol.Types (Position (..), Range (..))

-- | How to work on a backend's code: the language server to start, and the
-- language and file extension of the code.
data Backend = Backend
  { command :: [String]
  , languageId :: Text
  , extension :: String
  }
  deriving (Show)

instance Aeson.FromJSON Backend where
  parseJSON = Aeson.withObject "backend" $ \o ->
    Backend <$> o .: "command" <*> o .:? "languageId" .!= "" <*> o .:? "extension" .!= ""

-- | The backends whose code is handed to a language server unless configured otherwise.
defaultBackends :: Map.Map Text Backend
defaultBackends =
  Map.fromList
    [ ("GHC", Backend ["haskell-language-server-wrapper", "--lsp"] "haskell" "hs")
    , ("C", Backend ["clangd"] "c" "c")
    , ("CPP", Backend ["clangd"] "cpp" "cpp")
    , ("JS", Backend ["typescript-language-server", "--stdio"] "javascript" "js")
    ]

-- | A FOREIGN or COMPILE pragma: its backend and the lines of its code, each
-- with the position it starts at.
data Pragma = Pragma
  { compile :: Bool
  , backend :: Text
  , code :: [(Position, Text)]
  }

-- | The FOREIGN and COMPILE pragmas of a document.
pragmas :: Text -> [Pragma]
pragmas doc = go 0 doc
  where
    starts = Map.fromList (zip (scanl (\o l -> o + T.length l + 1) 0 docLines) (zip [0 ..] docLines))
    docLines = T.splitOn "\n" doc
    positionAt off = case Map.lookupLE off starts of
      Just (s, (l, line)) -> Position l (utf16Length (T.take (off - s) line))
      Nothing -> Position 0 0
    go off t = case T.breakOn "{-#" t of
      (_, "") -> []
      (before, rest) ->
        let start = off + T.length before + 3
            (inner, after) = T.breakOn "#-}" (T.drop 3 rest)
         in maybe id (:) (pragma start inner) (go (start + T.length inner + 3) (T.drop 3 after))
    -- @inner@ is what is between @{-#@ and @#-}@, at offset @start@
    pragma start inner = do
      let (kw, afterKw) = T.break isSpace (T.stripStart inner)
          (b, codeText) = T.break isSpace (T.stripStart afterKw)
          codeStart = start + T.length inner - T.length codeText
          pieces = T.splitOn "\n" codeText
          offsets = scanl (\o p -> o + T.length p + 1) codeStart pieces
      if kw `elem` ["FOREIGN", "COMPILE"] && not (T.null b)
        then Just (Pragma (kw == "COMPILE") b [(positionAt o, T.dropWhileEnd (== '\r') p) | (o, p) <- zip offsets pieces])
        else Nothing

-- | A line of the virtual file: generated text, then a piece of the
-- document's line that starts at @source@ (if it is not wholly generated).
-- Diagnostics are not reported on the lines of COMPILE expressions, which
-- the backend only sees inside the code it generates.
data Line = Line
  { prefix :: Text
  , source :: Maybe Position
  , body :: Text
  , checked :: Bool
  }

newtype Virtual = Virtual [Line]

-- | The virtual file of a backend's pragmas, in its language. Haskell needs
-- a module header (@name@) and its imports before the declarations; other
-- languages get the code as it is. Only GHC's COMPILE pragmas are taken,
-- those of the form @agdaName = expression@.
virtual :: Text -> Text -> Text -> [Pragma] -> Virtual
virtual language name b ps
  | language == "haskell" = Virtual (header : concat imports <> concat decls <> concat compiled)
  | otherwise = Virtual (concat items)
  where
    ours = filter ((== b) . backend) ps
    items = concatMap (blockItems . code) (filter (not . compile) ours)
    (imports, decls) = partition (any (("import" `T.isPrefixOf`) . body) . take 1) items
    header = Line ("module " <> name <> " where") Nothing "" False
    compiled = mapMaybe compiledItem (zip [0 :: Int ..] (filter compile ours))
    compiledItem (i, Pragma {code = (p, first) : rest}) = do
      let (agdaName, afterName) = T.break isSpace (T.stripStart first)
          eq = T.stripStart afterName
      expr <- T.stripPrefix "=" eq
      let exprText = T.stripStart expr
          col = _character p + utf16Length (T.take (T.length first - T.length exprText) first)
      if T.null agdaName || any (`T.isPrefixOf` exprText) ["type ", "data "]
        then Nothing
        else
          Just $
            Line ("agdaliciousCompile" <> T.pack (show i) <> " = ") (Just p {_character = col}) exprText False
              : [Line "  " (Just q) l False | (q, l) <- rest, not (T.all isSpace l)]
    compiledItem _ = Nothing

-- | The code of a block, split into items: an item starts on a line that is
-- indented least, and goes on over the lines indented more. The first line
-- (what follows the backend's name) always starts an item. Items are moved
-- to column 0.
blockItems :: [(Position, Text)] -> [[Line]]
blockItems [] = []
blockItems ((p, first) : rest) = group (firstLine <> map dedent nonBlank)
  where
    nonBlank = filter (not . T.all isSpace . snd) rest
    indent = T.length . T.takeWhile (== ' ')
    least = if null nonBlank then 0 else minimum (map (indent . snd) nonBlank)
    firstLine =
      [ Line "" (Just p {_character = _character p + utf16Length (T.takeWhile isSpace first)}) (T.strip first) True
      | not (T.all isSpace first)
      ]
    dedent (q, l) = Line "" (Just q {_character = _character q + fromIntegral least}) (T.stripEnd (T.drop least l)) True
    group [] = []
    group (l : ls) = let (more, next) = break startsItem ls in (l : more) : group next
    startsItem l = not (T.null (body l)) && not (isSpace (T.head (body l)))

-- | The virtual file's text.
text :: Virtual -> Text
text (Virtual ls) = T.unlines [prefix l <> body l | l <- ls]

-- | The position in the virtual file of a position in the document, if it
-- is in foreign code.
toVirtual :: Virtual -> Position -> Maybe Position
toVirtual (Virtual ls) (Position l c) =
  listToMaybe
    [ Position i (utf16Length (prefix line) + c - _character s)
    | (i, line) <- zip [0 ..] ls
    , Just s <- [source line]
    , _line s == l
    , _character s <= c
    , c <= _character s + utf16Length (body line)
    ]

-- | The position in the document of a position in the virtual file, if it
-- is not in generated text.
fromVirtual :: Virtual -> Position -> Maybe Position
fromVirtual v p = fst <$> fromVirtual' v p

fromVirtual' :: Virtual -> Position -> Maybe (Position, Bool)
fromVirtual' (Virtual ls) (Position l c) = case drop (fromIntegral l) ls of
  line : _ | Just s <- source line ->
    let n = utf16Length (prefix line)
        off = min (utf16Length (body line)) (if c < n then 0 else c - n)
     in Just (s {_character = _character s + off}, checked line)
  _ -> Nothing

-- | A range in the virtual file as one in the document. A range that ends
-- at the start of a line ends at the end of the line before, which may come
-- from somewhere else in the document.
fromVirtualRange :: Virtual -> Range -> Maybe Range
fromVirtualRange v r = fst <$> fromVirtualRange' v r

fromVirtualRange' :: Virtual -> Range -> Maybe (Range, Bool)
fromVirtualRange' v (Range s e) = do
  (s', ok) <- fromVirtual' v s
  let e'
        | _character e == 0 && _line e > _line s = Position (_line e - 1) maxBound
        | otherwise = e
  (e'', ok') <- fromVirtual' v e'
  pure (Range s' e'', ok && ok')

-- | Where a diagnostic of the virtual file is to be reported in the
-- document, if it is.
reported :: Virtual -> Range -> Maybe Range
reported v r = case fromVirtualRange' v r of
  Just (r', True) -> Just r'
  _ -> Nothing

utf16Length :: Integral n => Text -> n
utf16Length = fromIntegral . T.foldl' (\n ch -> n + if fromEnum ch > 0xFFFF then 2 else 1) (0 :: Int)
