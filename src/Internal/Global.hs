module Internal.Global where

import Control.Concurrent
import Control.Lens hiding ((.=))
import qualified Data.HashMap.Strict as HashMap

import Import

import Internal.WS

globalEvents :: App -> IO ()
globalEvents App {..} = void $ forkIO $ forever $ atomically $ do
  globalEvent <- readTQueue tqGlobalEvents
  HashMap.elems <$> readTVar tvWSConns >$>= \(pubsub, tqueue) ->
    case globalEvent of
      MainChat _ | receiveMainChat pubsub ->
        writeTQueue tqueue globalEvent
      StreamStatus _ | receiveStreamStatus pubsub ->
        writeTQueue tqueue globalEvent
      ModEvent _ | recieveModEvents pubsub ->
        writeTQueue tqueue globalEvent
      _ -> return ()


streamStatusEventTimer :: App -> IO ()
streamStatusEventTimer App {..} = void $ forkIO $ forever $ do
  threadDelay $ 4 * minutes
  atomically $ do
    numOfUsers <- HashMap.size <$> readTVar tvUserConns
    streamStatus <- set viewerCount numOfUsers <$> readTVar tvStreamStatus
    writeTVar tvStreamStatus streamStatus
    writeTQueue tqGlobalEvents $ StreamStatus streamStatus

streamStatistics :: App -> IO ()
streamStatistics App {..} = void $ forkIO $ forever $ do
  threadDelay $ 5 * minutes
