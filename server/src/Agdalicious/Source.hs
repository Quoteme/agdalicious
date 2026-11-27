-- | Documentation read from Agda sources, for names in any library: what a
-- declaration says about itself, following the conventions of the standard
-- library.
--
-- * The comment just above a declaration (with at most one blank line, or
--   fixity and other pragmas, in between) documents it.
-- * A comment right under a line of dashes is the heading of the part of the
--   file that follows.
-- * The comment at the top of a file documents the module.
module Agdalicious.Source
  ( Declaration (..)
  , declaration
  , definedName
  , Module (..)
  , moduleOf
  , builtinOf
  , Library (..)
  , libraryOf
  ) where

import Control.Exception (IOException, try)
import Data.Char (isSpace)
import Data.List (find, isSuffixOf)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO.Utf8 qualified as T
import System.Directory (listDirectory)
import System.FilePath (takeDirectory, (</>))

data Declaration = Declaration
  { code :: [Text]
  -- ^ the declaration, or only its type signature if it is long
  , complete :: Bool
  -- ^ whether 'code' is all of it
  , doc :: [Text]
  -- ^ the comment above it, one paragraph per element
  , section :: Maybe Text
  -- ^ the heading of the part of the file it is in
  }

maxLines :: Int
maxLines = 12

-- | The declaration on a line (counted from 0) of a file's lines.
declaration :: Int -> [Text] -> Declaration
declaration n ls = Declaration {code = if complete then decl else signature, complete, doc, section}
  where
    (above, here) = splitAt n ls
    first = case here of
      l : _ -> l
      [] -> ""
    depth = indentation first
    -- the lines that belong to it: indented further, or at its level but not
    -- the next signature, fixity or pragma
    rest = takeWhile continues (drop 1 here)
    continues l =
      not (blank l) && (indentation l > depth || (indentation l == depth && not (startsDeclaration l)))
    decl = first : rest
    complete = length decl <= maxLines
    signature = take maxLines (first : takeWhile (\l -> not (blank l) && indentation l > depth) (drop 1 here))
    -- above it, nearest first
    up = reverse above
    doc = case comments (dropWhile skippable (dropBlank up)) of
      (_, next : _) | isRule next -> [] -- that is a heading
      (block, _) -> paragraphs (reverse block)
    section = listToMaybe [commentText c | (c, r) <- zip up (drop 1 up), isComment c, not (isRule c), isRule r]
    dropBlank (l : xs) | blank l = xs
    dropBlank xs = xs
    skippable l = any (`T.isPrefixOf` T.stripStart l) ["infix", "{-#"]
    comments xs = span (\l -> isComment l && not (isRule l)) xs

-- | The name declared at an offset of a file's text, e.g. @_+_@.
definedName :: Int -> Text -> Text
definedName off = T.takeWhile (\c -> not (isSpace c || c `elem` (".;{}()@\"" :: String))) . T.drop (off - 1)

data Module = Module
  { moduleName :: Maybe Text
  , header :: [Text]
  -- ^ the comment at the top, one paragraph per element
  }

moduleOf :: [Text] -> Module
moduleOf ls =
  Module
    { moduleName = listToMaybe (mapMaybe (fmap (T.takeWhile (not . isSpace) . T.stripStart) . T.stripPrefix "module ") ls)
    , header = paragraphs (takeWhile (\l -> blank l || isComment l) ls)
    }

-- | The built-in a name is bound to (@BUILTIN NATURAL@), by a pragma in its file.
builtinOf :: Text -> [Text] -> Maybe Text
builtinOf name ls =
  listToMaybe
    [ "BUILTIN " <> b
    | l <- ls
    , ["{-#", "BUILTIN", b, n, "#-}"] <- [T.words l]
    , n == name
    ]

data Library = Library {libraryName, libraryVersion :: Text}

-- | The library a file belongs to: from the @.agda-lib@ file in its directory
-- or the nearest one above, whose @name:@ is @standard-library-2.4@ or similar.
libraryOf :: FilePath -> IO (Maybe Library)
libraryOf = go . takeDirectory
  where
    go dir = do
      entries <- either (const []) id <$> (try (listDirectory dir) :: IO (Either IOException [FilePath]))
      case find (".agda-lib" `isSuffixOf`) entries of
        Just lib -> parse <$> T.readFile (dir </> lib)
        Nothing | takeDirectory dir /= dir -> go (takeDirectory dir)
        Nothing -> pure Nothing
    parse t = do
      name <- listToMaybe (mapMaybe (fmap T.strip . T.stripPrefix "name:") (T.lines t))
      let (n, v) = T.breakOnEnd "-" name
      pure $
        if not (T.null n) && T.all (`elem` ("0123456789." :: String)) v && not (T.null v)
          then Library (T.dropEnd 1 n) v
          else Library name ""

-- * Comments

blank :: Text -> Bool
blank = T.all isSpace

indentation :: Text -> Int
indentation = T.length . T.takeWhile (== ' ')

isComment :: Text -> Bool
isComment = ("--" `T.isPrefixOf`) . T.stripStart

-- | A line of dashes.
isRule :: Text -> Bool
isRule l = let s = T.strip l in T.length s >= 4 && T.all (== '-') s

commentText :: Text -> Text
commentText = T.strip . T.dropWhile (== '-') . T.stripStart

startsDeclaration :: Text -> Bool
startsDeclaration l = case T.breakOn " = " l of
  (before, _) -> " : " `T.isInfixOf` before || any (`T.isPrefixOf` T.stripStart l) ["infix", "{-#"]

-- | The text of comment lines, as paragraphs.
paragraphs :: [Text] -> [Text]
paragraphs =
  filter (not . T.null) . map (T.unwords . map commentText) . splitOn (\l -> blank l || T.null (commentText l)) . filter (not . isRule)
  where
    splitOn p xs = case break p xs of
      (a, _ : b) -> a : splitOn p b
      (a, []) -> [a]
