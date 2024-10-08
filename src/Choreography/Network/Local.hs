-- | This module defines the multi-thread backend for the `Network` monad.
module Choreography.Network.Local where

import Control.Concurrent
import Control.Monad
import Control.Monad.Freer
import Control.Monad.IO.Class
import Data.HashMap.Strict (HashMap, (!), (!?))
import Data.HashMap.Strict qualified as HashMap

import Choreography.Locations
import Choreography.Network

-- | Each location is associated with a message buffer which stores messages sent
-- from other locations.
type MsgBuf = HashMap LocTm (Chan String)

newtype LocalConfig = LocalConfig
  { locToBuf :: HashMap LocTm MsgBuf
  }

newEmptyMsgBuf :: [LocTm] -> IO MsgBuf
newEmptyMsgBuf = foldM f HashMap.empty
  where
    f hash loc = do
      chan <- newChan
      return (HashMap.insert loc chan hash)

mkLocalConfig :: [LocTm] -> IO LocalConfig
mkLocalConfig ls = LocalConfig <$> foldM f HashMap.empty ls
  where
    f hash loc = do
      buf <- newEmptyMsgBuf ls
      return (HashMap.insert loc buf hash)

locs :: LocalConfig -> [LocTm]
locs = HashMap.keys . locToBuf

runNetworkLocal :: MonadIO m => LocalConfig -> LocTm -> Network m a -> m a
runNetworkLocal cfg self = interpFreer handler
  where
    handler :: MonadIO m => NetworkSig m a -> m a
    handler (Run m)    = m
    handler (Send a ls) = liftIO $ mapM_ (\l -> writeChan ((locToBuf cfg ! l) ! self) (show a)) ls
    handler (Recv l)   = do let b = locToBuf cfg ! self
                            let q = b !? l
                            case q of
                              Just q' -> liftIO $ read <$> readChan q'
                              Nothing -> liftIO do print $ void b
                                                   print l
                                                   error $ "We don't know how to contact the party named \"" ++ l ++ "\"."

instance Backend LocalConfig where
  runNetwork = runNetworkLocal

