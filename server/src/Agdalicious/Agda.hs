-- | A persistent @agda --interaction-json@ process (see @docs/agda-interaction.md@).
--
-- Keeping one process alive lets Agda reuse already-checked imports, so
-- reloading a file only re-checks what changed.
module Agdalicious.Agda (Agda, start, command) where

import Control.Concurrent.MVar
import Control.Exception (onException)
import Control.Monad (unless)
import Data.Aeson qualified as Aeson
import Data.List (isInfixOf, isPrefixOf)
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
--
-- Cancelling the calling thread (e.g. with 'Control.Concurrent.Async.cancel')
-- aborts the command in Agda too, and leaves the process ready for the next one.
command :: Agda -> FilePath -> String -> IO [Aeson.Value]
command (Agda lock) file cmd = withMVar lock $ \(i, o) -> do
  let send c = hPutStrLn i ("IOTCM " <> show file <> " None Direct (" <> c <> ")") >> hFlush i
      -- An idle Cmd_abort prints nothing, so mark the end with a version request.
      resync = send "Cmd_abort" >> send "Cmd_show_version" >> drain
      drain = untilPrompt o >>= \out -> unless ("\"kind\":\"Version\"" `isInfixOf` out) drain
  send cmd
  out <- untilPrompt o `onException` resync
  pure (mapMaybe (Aeson.decode . TL.encodeUtf8 . TL.pack) (lines out))

-- | Read up to the next @JSON> @ prompt, which ends every batch of responses.
untilPrompt :: Handle -> IO String
untilPrompt h = go ""
  where
    go acc
      | " >NOSJ" `isPrefixOf` acc = pure (reverse (drop 6 acc))
      | otherwise = hGetChar h >>= go . (: acc)
