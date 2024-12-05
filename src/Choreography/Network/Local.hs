-- | This module defines the multi-thread backend for the `Network` monad.
module Choreography.Network.Local where

import Choreography.Locations
import Choreography.Network
import Control.Concurrent
import Control.Monad
import Control.Monad.Freer
import Control.Monad.IO.Class
import Data.HashMap.Strict (HashMap, (!), (!?))
import Data.HashMap.Strict qualified as HashMap

-- | Each location is associated with a message buffer which stores messages sent
-- from other locations.
type MsgBuf = HashMap LocTm (Chan String)

-- | A backend for running choreographies using [Haskell threads](https://hackage.haskell.org/package/base/docs/Control-Concurrent.html)
--   as the locations and buffered `Control.Concurrent.Chan.Chan` channels for communication.
newtype LocalConfig = LocalConfig
  { locToBuf :: HashMap LocTm MsgBuf
  }

-- | Make a channel for each of the listed locations, on which messages from that location can be recieved.
newEmptyMsgBuf :: [LocTm] -> IO MsgBuf
newEmptyMsgBuf = foldM f HashMap.empty
  where
    f hash loc = do
      chan <- newChan
      pure (HashMap.insert loc chan hash)

-- | Make a local backend for the listed parties.
--   Make just the one backend and then have all your threads use the same one.
mkLocalConfig :: [LocTm] -> IO LocalConfig
mkLocalConfig ls = LocalConfig <$> foldM f HashMap.empty ls
  where
    f hash loc = do
      buf <- newEmptyMsgBuf ls
      pure (HashMap.insert loc buf hash)

-- | List the parties known to the backend.
locs :: LocalConfig -> [LocTm]
locs = HashMap.keys . locToBuf

-- | Run a `Network` behavior using the channels in a t`LocalConfig` for communication.
--   Call this inside a concurrent thread.
runNetworkLocal :: (MonadIO m) => LocalConfig -> LocTm -> Network m a -> m a
runNetworkLocal cfg self = interpFreer handler
  where
    handler :: (MonadIO m) => NetworkSig m a -> m a
    handler (Run m) = m
    handler (Send a ls) = liftIO $ mapM_ (\l -> writeChan ((locToBuf cfg ! l) ! self) (show a)) ls
    handler (Recv l) = do
      let b = locToBuf cfg ! self
      let q = b !? l
      case q of
        Just q' -> liftIO $ read <$> readChan q'
        Nothing -> liftIO do
          print $ void b
          print l
          error $ "We don't know how to contact the party named \"" <> l <> "\"."

instance Backend LocalConfig where
  runNetwork = runNetworkLocal
