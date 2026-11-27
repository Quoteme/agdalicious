-- | Where Agda finds the modules a file can import, worked out the way Agda
-- does it: from the file's library (its @.agda-lib@), or without one from the
-- file's own root and the default libraries; then the libraries those depend
-- on, found through the libraries file in Agda's application directory; and
-- Agda's built-in modules.
module Agdalicious.Libraries (Config, config, includes, rootOf, modulesIn) where

import Control.Exception (IOException, try)
import Control.Monad (filterM, foldM)
import Data.Char (isSpace)
import Data.List (find, isInfixOf, isPrefixOf, isSuffixOf, nub)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO.Utf8 qualified as T
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (dropExtensions, takeDirectory, (</>))
import System.Process (readProcess)

-- | Agda's application directory (with the libraries and defaults files) and
-- its data directory (with the built-in modules).
data Config = Config {appDir, dataDir :: FilePath}

config :: IO Config
config = Config <$> ask "--print-agda-app-dir" <*> ask "--print-agda-data-dir"
  where
    ask flag = either (const "") (takeWhile (/= '\n')) <$> tryIO (readProcess "agda" [flag] "")

-- | The directories the modules a file can import are in. The file's module
-- name (@Foo.Bar@) gives its root when it isn't in a library.
includes :: Config -> FilePath -> Maybe Text -> IO [FilePath]
includes cfg file name = do
  known <- installed cfg
  own <- findUp (takeDirectory file)
  (dirs, deps) <- case own of
    Just lib -> fields lib
    Nothing -> (,) [rootOf file name] . words <$> readOr (appDir cfg </> "defaults")
  rest <- resolve known [] deps
  filterM doesDirectoryExist (nub (dirs <> rest <> [dataDir cfg </> "lib" </> "prim"]))

-- | The directory a file's module name is relative to: for @Foo.Bar@ in
-- @/x/Foo/Bar.agda@, @/x@.
rootOf :: FilePath -> Maybe Text -> FilePath
rootOf file name = iterate takeDirectory (takeDirectory file) !! depth
  where
    depth = maybe 0 (subtract 1 . length . T.splitOn ".") name

-- | The include directories of some libraries and of those they depend on.
resolve :: [(String, FilePath)] -> [String] -> [String] -> IO [FilePath]
resolve known seen = foldM dep [] . filter (`notElem` seen)
  where
    dep acc d = case find (matches d . fst) known of
      Nothing -> pure acc
      Just (_, lib) -> do
        (dirs, more) <- fields lib
        (\r -> acc <> dirs <> r) <$> resolve known (d : seen) more
    -- "standard-library" is any version of it, "standard-library-2.4" that one
    matches d n = d == n || ((d <> "-") `isPrefixOf` n && all (`elem` ("0123456789." :: String)) (drop (length d + 1) n))

-- | The modules in a directory and the ones under it, with their files.
modulesIn :: FilePath -> IO [(Text, FilePath)]
modulesIn = go []
  where
    go prefix dir = do
      entries <- either (const []) id <$> tryIO (listDirectory dir)
      concat <$> mapM (entry prefix dir) entries
    entry prefix dir e
      | take 1 e `elem` ["_", "."] = pure [] -- _build, .git, ...
      | ".agda" `isSuffixOf` e || ".lagda" `isSuffixOf` e || ".lagda." `isInfixOf` e =
          pure [(T.intercalate "." (map T.pack (prefix <> [dropExtensions e])), dir </> e)]
      | otherwise = do
          isDir <- doesDirectoryExist (dir </> e)
          if isDir then go (prefix <> [e]) (dir </> e) else pure []

-- * Library files

-- | The installed libraries, by name, from the libraries files
-- (@libraries@, @libraries-2.8.0@, ...): each line names an @.agda-lib@ file.
installed :: Config -> IO [(String, FilePath)]
installed cfg = do
  files <- either (const []) id <$> tryIO (listDirectory (appDir cfg))
  libs <- concatMap paths <$> mapM (readOr . (appDir cfg </>)) (filter ("libraries" `isPrefixOf`) files)
  concat <$> mapM named libs
  where
    paths = filter (\l -> not (null l) && not ("--" `isPrefixOf` l)) . map trim . lines
    named l = do
      fs <- parse <$> readOr l
      pure [(n, l) | Just n <- [lookup "name" fs]]

-- | A library's include directories and the libraries it depends on.
fields :: FilePath -> IO ([FilePath], [String])
fields lib = do
  fs <- parse <$> readOr lib
  let get k = concat [words (map (\c -> if c == ',' then ' ' else c) v) | (k', v) <- fs, k' == k]
  pure (map (takeDirectory lib </>) (get "include"), get "depend")

-- | The fields of a library file. An indented line continues the field
-- before it; @--@ starts a comment.
parse :: String -> [(String, String)]
parse = foldr field [] . filter (not . all isSpace) . map uncomment . lines
  where
    uncomment = T.unpack . fst . T.breakOn "--" . T.pack
    field l acc
      | take 1 l `elem` [" ", "\t"], (k, v) : rest <- acc = (k, trim l <> " " <> v) : rest
      | (k, ':' : v) <- break (== ':') l = (trim k, trim v) : acc
      | otherwise = acc

-- | The nearest @.agda-lib@ file in a directory or above it.
findUp :: FilePath -> IO (Maybe FilePath)
findUp dir = do
  entries <- either (const []) id <$> tryIO (listDirectory dir)
  case filter (".agda-lib" `isSuffixOf`) entries of
    lib : _ -> pure (Just (dir </> lib))
    [] | takeDirectory dir /= dir -> findUp (takeDirectory dir)
    [] -> pure Nothing

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

readOr :: FilePath -> IO String
readOr f = either (const "") T.unpack <$> tryIO (T.readFile f)

tryIO :: IO a -> IO (Either IOException a)
tryIO = try
