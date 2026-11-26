-- | A mirror of the file system in which open documents are real files with
-- their editor contents, and everything else is a symlink to the original.
--
-- Agda only reads files from disk, so checking a document's copy in the mirror
-- checks its unsaved contents, against the unsaved contents of the open
-- documents it imports. Directories become real (instead of links) only on
-- the way to an open document, so the mirror stays small.
module Agdalicious.Shadow (Shadow, with, path, unshadow, write, restore) where

import Control.Exception (bracket)
import Control.Monad (unless, when)
import Data.Text qualified as T
import Data.Text.IO.Utf8 qualified as T
import System.Directory
import System.FilePath
import System.Posix.Process (getProcessID)

newtype Shadow = Shadow FilePath

-- | Run with a fresh mirror, removed afterwards.
with :: (Shadow -> IO a) -> IO a
with = bracket create (\(Shadow dir) -> removePathForcibly dir)
  where
    create = do
      dir <- (</>) <$> getTemporaryDirectory <*> (("agdalicious-" <>) . show <$> getProcessID)
      removePathForcibly dir >> createDirectory dir >> mirror "/" dir
      pure (Shadow dir)

-- | The mirror's copy of an absolute path.
path :: Shadow -> FilePath -> FilePath
path (Shadow dir) f = dir </> makeRelative "/" f

-- | Turn the mirror's paths in a message back into the originals.
unshadow :: Shadow -> T.Text -> T.Text
unshadow (Shadow dir) = T.replace (T.pack dir) ""

-- | Give the mirror's copy of a file these contents.
write :: Shadow -> FilePath -> T.Text -> IO ()
write s f t = do
  expand s (takeDirectory f)
  removePathForcibly (path s f) -- the link: never write through to the original
  T.writeFile (path s f) t

-- | Point the mirror's copy of a file back at the original.
restore :: Shadow -> FilePath -> IO ()
restore s f = removePathForcibly (path s f) >> createFileLink f (path s f)

-- | Make the mirror's copy of a directory (and its parents) real.
expand :: Shadow -> FilePath -> IO ()
expand s d = unless (takeDirectory d == d) $ do
  expand s (takeDirectory d)
  isLink <- pathIsSymbolicLink (path s d)
  when isLink $ removeFile (path s d) >> createDirectory (path s d) >> mirror d (path s d)

-- | Fill @copy@ with links to the entries of @orig@.
mirror :: FilePath -> FilePath -> IO ()
mirror orig copy = listDirectory orig >>= mapM_ (\e -> createFileLink (orig </> e) (copy </> e))
