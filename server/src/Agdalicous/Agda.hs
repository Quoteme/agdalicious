-- | A persistent @agda --interaction-json@ process (see @docs/agda-interaction.md@).
--
-- Keeping one process alive lets Agda reuse already-checked imports, so
-- reloading a file only re-checks what changed.
module Agdalicous.Agda (Agda, start, command) where

import Control.Concurrent.MVar
import Data.Aeson qualified as Aeson
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import System.IO
import System.Process

-- | stdin/stdout of the process; the lock makes commands run one at a time.
newtype Agda = Agda (MVar (Handle, Handle))

start :: IO Agda
start = do
  (Just i, Just o, _, _) <-
    createProcess (proc "agda" ["--interaction-json"]) {std_in = CreatePipe, std_out = CreatePipe}
  mapM_ (`hSetEncoding` utf8) [i, o]
  _ <- untilPrompt o
  Agda <$> newMVar (i, o)

-- | Run an interaction command (e.g. @Cmd_load "/x/A.agda" []@) for a file
-- and return Agda's responses.
command :: Agda -> FilePath -> String -> IO [Aeson.Value]
command (Agda lock) file cmd = withMVar lock $ \(i, o) -> do
  hPutStrLn i ("IOTCM " <> show file <> " None Direct (" <> cmd <> ")") >> hFlush i
  mapMaybe (Aeson.decode . TL.encodeUtf8 . TL.pack) . lines <$> untilPrompt o

-- | Read up to the next @JSON> @ prompt, which ends every batch of responses.
untilPrompt :: Handle -> IO String
untilPrompt h = go ""
  where
    go acc
      | " >NOSJ" `isPrefixOf` acc = pure (reverse (drop 6 acc))
      | otherwise = hGetChar h >>= go . (: acc)
