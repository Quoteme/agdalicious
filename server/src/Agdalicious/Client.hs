-- | A language server of another language (the one of a backend's foreign
-- code, see "Agdalicious.Foreign"), which we drive as its client over stdio.
module Agdalicious.Client (Client, start, request, notify) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (race_)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception (IOException, SomeException, handle, try)
import Control.Lens ((^..), (^?))
import Control.Monad (forever, void)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key, values, _Integral, _String)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as BL
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import System.IO
import System.Process

data Client = Client
  { outbox :: Chan Aeson.Value
  -- ^ messages to send, once it is initialized
  , pending :: MVar (Map.Map Int (MVar (Either Aeson.Value Aeson.Value)))
  -- ^ the requests waiting for their response (an error or a result)
  , counter :: IORef Int
  , alive :: IORef Bool
  }

-- | Start a language server (@command@) in a directory and initialize it
-- with these parameters. Its notifications go to @onNotify@; its requests
-- get empty answers. Messages sent before it is initialized wait for that.
-- If it stops (or sends what is not a message), @onExit@ gets the last
-- lines of its log, which say why. 'Nothing' if it cannot be started.
start :: [String] -> FilePath -> Aeson.Value -> (Text -> Aeson.Value -> IO ()) -> ([Text] -> IO ()) -> IO (Maybe Client)
start [] _ _ _ _ = pure Nothing
start (cmd : args) dir initParams onNotify onExit = handle (\(_ :: IOException) -> pure Nothing) $ do
  (Just i, Just o, Just e, _) <-
    createProcess (proc cmd args) {cwd = Just dir, std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
  mapM_ (`hSetBinaryMode` True) [i, o]
  c <- Client <$> newChan <*> newMVar mempty <*> newIORef 0 <*> newIORef True
  logTail <- newIORef []
  logDone <- newEmptyMVar
  void . forkIO $ do
    ignoreErrors . forever $ B8.hGetLine e >>= \l -> modifyIORef' logTail (take 20 . (l :))
    putMVar logDone ()
  void . forkIO $ reader c o (race_ (takeMVar logDone) (threadDelay 2000000) >> reverse <$> readIORef logTail)
  void . forkIO . ignoreErrors $ do
    _ <- call c i "initialize" initParams
    write i (Aeson.object ["jsonrpc" .= ("2.0" :: Text), "method" .= ("initialized" :: Text), "params" .= Aeson.object []])
    forever (readChan (outbox c) >>= write i)
  pure (Just c)
  where
    ignoreErrors = handle (\(_ :: SomeException) -> pure ())
    -- @lastLog@: the rest of its log, once it is exiting
    reader c o lastLog = do
      r <- try (readMessage o)
      case r of
        Left (_ :: SomeException) -> do
          writeIORef (alive c) False
          ps <- swapMVar (pending c) mempty
          mapM_ (`putMVar` Left Aeson.Null) ps
          onExit . map (withoutColours . decodeUtf8Lenient) =<< lastLog
        Right msg -> do
          case (msg ^? key "id", msg ^? key "method" . _String) of
            (Just i, Just m) -> writeChan (outbox c) (reply i m msg)
            (Just i, Nothing) | Just n <- i ^? _Integral -> do
              waiting <- modifyMVar (pending c) (\ps -> pure (Map.delete n ps, Map.lookup n ps))
              mapM_ (`putMVar` maybe (Right (maybe Aeson.Null id (msg ^? key "result"))) Left (msg ^? key "error")) waiting
            (Nothing, Just m) -> ignoreErrors (onNotify m (maybe Aeson.Null id (msg ^? key "params")))
            _ -> pure ()
          reader c o lastLog
    withoutColours t = case T.breakOn "\ESC[" t of
      (before, "") -> before
      (before, rest) -> before <> withoutColours (T.drop 1 (T.dropWhile (/= 'm') rest))
    -- @workspace/configuration@ wants one (here: no) setting per item asked for
    reply i m msg =
      Aeson.object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "id" .= i
        , "result" .= case m of
            "workspace/configuration" -> Aeson.toJSON [Aeson.Null | _ <- msg ^.. key "params" . key "items" . values]
            _ -> Aeson.Null
        ]
    -- the initialize request, written before anything else
    call c i m params = do
      (n, wait) <- register c
      write i (requestMessage n m params)
      takeMVar wait

-- | Send a request and wait for its result (or error).
request :: Client -> Text -> Aeson.Value -> IO (Either Aeson.Value Aeson.Value)
request c m params = do
  up <- readIORef (alive c)
  if not up
    then pure (Left Aeson.Null)
    else do
      (n, wait) <- register c
      writeChan (outbox c) (requestMessage n m params)
      takeMVar wait

notify :: Client -> Text -> Aeson.Value -> IO ()
notify c m params = writeChan (outbox c) (Aeson.object ["jsonrpc" .= ("2.0" :: Text), "method" .= m, "params" .= params])

register :: Client -> IO (Int, MVar (Either Aeson.Value Aeson.Value))
register c = do
  n <- atomicModifyIORef' (counter c) (\k -> (k + 1, k + 1))
  wait <- newEmptyMVar
  modifyMVar_ (pending c) (pure . Map.insert n wait)
  pure (n, wait)

requestMessage :: Int -> Text -> Aeson.Value -> Aeson.Value
requestMessage n m params = Aeson.object ["jsonrpc" .= ("2.0" :: Text), "id" .= n, "method" .= m, "params" .= params]

write :: Handle -> Aeson.Value -> IO ()
write h v = do
  let b = Aeson.encode v
  B8.hPut h ("Content-Length: " <> B8.pack (show (BL.length b)) <> "\r\n\r\n")
  BL.hPut h b
  hFlush h

readMessage :: Handle -> IO Aeson.Value
readMessage h = headers Nothing
  where
    headers len = do
      l <- B8.strip <$> B8.hGetLine h
      case B8.stripPrefix "Content-Length:" l of
        _ | B.null l, Just n <- len -> B.hGet h n >>= maybe (fail "bad message") pure . Aeson.decodeStrict
        Just n | Just (k, _) <- B8.readInt (B8.strip n) -> headers (Just k)
        _ -> headers len
