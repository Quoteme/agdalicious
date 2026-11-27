-- | A persistent @agda --interaction-json@ process (see @docs/agda-interaction.md@).
--
-- Keeping one process alive lets Agda reuse already-checked imports, so
-- reloading a file only re-checks what changed.
module Agdalicious.Agda (Agda, start, command, knows) where

import Control.Concurrent.MVar
import Control.Exception (onException)
import Control.Lens ((^?))
import Control.Monad (unless, when)
import Data.IORef
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key, _String)
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import System.Directory (removeFile)
import System.IO
import System.Process

-- | stdin/stdout of the process, and the file it loaded last (if it is
-- certain); the lock makes commands run one at a time.
newtype Agda = Agda (MVar (Handle, Handle, IORef (Maybe FilePath)))

start :: IO Agda
start = do
  (Just i, Just o, _, _) <-
    createProcess (proc "agda" ["--interaction-json"]) {std_in = CreatePipe, std_out = CreatePipe}
  mapM_ (`hSetEncoding` utf8) [i, o]
  _ <- untilPrompt o
  Agda <$> (newMVar . (i,o,) =<< newIORef Nothing)

-- | Run an interaction command (e.g. @Cmd_load "/x/A.agda" []@) for a file
-- and return Agda's responses, with highlighting (@info.payload@ of each
-- @HighlightingInfo@) for what it checked.
--
-- Other commands (goal and top-level ones, e.g. @Cmd_infer_toplevel AsIs
-- "_+_"@) act on the file loaded last: if that is another one, the file is
-- loaded first.
--
-- Cancelling the calling thread (e.g. with 'Control.Concurrent.Async.cancel')
-- aborts the command in Agda too, and leaves the process ready for the next one.
command :: Agda -> FilePath -> String -> IO [Aeson.Value]
command (Agda lock) file cmd = withMVar lock $ \(i, o, loaded) -> do
  let send c = hPutStrLn i ("IOTCM " <> show file <> " NonInteractive Indirect (" <> c <> ")") >> hFlush i
      batch = untilPrompt o >>= parse
      -- An idle Cmd_abort prints nothing, so mark the end with a version request.
      resync = send "Cmd_abort" >> send "Cmd_show_version" >> drain
      drain = batch >>= \rs -> unless (any (\r -> r ^? key "info" . key "kind" == Just "Version") rs) drain
      run c
        | "Cmd_load" `isPrefixOf` c = do
            writeIORef loaded Nothing -- until it is done
            rs <- send c >> batch `onException` resync
            rs <$ writeIORef loaded (Just file)
        | otherwise = send c >> batch `onException` resync
  current <- readIORef loaded
  when (not ("Cmd_load" `isPrefixOf` cmd) && current /= Just file) $
    () <$ run ("Cmd_load " <> show file <> " []")
  run cmd

-- | Whether this Agda knows a command (for a goal: @Cmd_give WithoutForce 0
-- noRange ""@): one it can't read, it says so instead of running it. It is
-- run for a file that does not exist, so that it changes nothing (and the
-- next command loads its file again).
knows :: Agda -> String -> IO Bool
knows (Agda lock) cmd = withMVar lock $ \(i, o, loaded) -> do
  hPutStrLn i ("IOTCM \"/agdalicious-probe.agda\" None Direct (" <> cmd <> ")") >> hFlush i
  out <- untilPrompt o
  writeIORef loaded Nothing
  pure (not ("cannot read" `isInfixOf` out))

-- | Decode a batch of responses. Highlighting comes in temporary files
-- (@Indirect@, which keeps the pipe small): read them in and delete them.
parse :: String -> IO [Aeson.Value]
parse = traverse inline . mapMaybe (Aeson.decode . TL.encodeUtf8 . TL.pack) . lines
  where
    inline r = case (r ^? key "kind" . _String, T.unpack <$> r ^? key "filepath" . _String) of
      (Just "HighlightingInfo", Just f) -> do
        info <- Aeson.decodeFileStrict f :: IO (Maybe Aeson.Value)
        removeFile f
        pure (Aeson.object ["kind" .= ("HighlightingInfo" :: T.Text), "info" .= info])
      _ -> pure r

-- | Read up to the next @JSON> @ prompt, which ends every batch of responses.
untilPrompt :: Handle -> IO String
untilPrompt h = go ""
  where
    go acc
      | " >NOSJ" `isPrefixOf` acc = pure (reverse (drop 6 acc))
      | otherwise = hGetChar h >>= go . (: acc)
