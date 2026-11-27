-- | Finding the commit at compile time, for "Version" (a splice cannot use
-- what its own module defines).
module Version.Commit (found) where

import Control.Exception (SomeException, try)
import Control.Monad (filterM)
import Language.Haskell.TH (Q, runIO)
import Language.Haskell.TH.Syntax (addDependentFile)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.Process (readProcess)

found :: Q String
found = do
  given <- runIO (lookupEnv "AGDALICIOUS_COMMIT")
  case given of
    Just c | not (null c) -> pure c
    _ -> do
      rev <- runIO (git ["rev-parse", "HEAD"])
      dirty <- runIO (git ["status", "--porcelain", "--untracked-files=no"])
      -- recompile after a commit or a checkout
      dir <- runIO (git ["rev-parse", "--absolute-git-dir"])
      ref <- runIO (git ["symbolic-ref", "-q", "HEAD"])
      files <- runIO (filterM doesFileExist [d <> "/" <> f | Just d <- [dir], Just f <- [Just "HEAD", ref]])
      mapM_ addDependentFile files
      pure $ case rev of
        Just r -> r <> (if maybe False (not . null) dirty then "-dirty" else "")
        Nothing -> "unknown"

-- | The output of a git command (its first line), if it succeeds.
git :: [String] -> IO (Maybe String)
git args = either (const Nothing) (Just . concat . take 1 . lines) <$> (try (readProcess "git" args "") :: IO (Either SomeException String))
