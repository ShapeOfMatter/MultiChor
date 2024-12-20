-- | This module defines the `Network` monad, which represents programs run on
-- individual nodes in a distributed system with explicit sends and receives.
-- To run a `Network` program, we provide a `runNetwork` function that supports
-- multiple message transport backends.
-- Two such backends are provided in "Choreography.Network.Http" and "Choreography.Network.Local",
-- and there should be enough tools here for you to write more as needed.
module Choreography.Network where

import Choreography.Locations (LocTm)
import Control.Monad.Freer
import Control.Monad.IO.Class

-- * The Network monad

-- | Effect signature for the `Network` monad.
data NetworkSig m a where
  -- | Local computation.
  Run ::
    m a ->
    NetworkSig m a
  -- | Sending.
  Send ::
    (Show a) =>
    a ->
    [LocTm] ->
    NetworkSig m ()
  -- | Receiving.
  Recv ::
    (Read a) =>
    LocTm ->
    NetworkSig m a

-- | Monad that represents network programs.
type Network m = Freer (NetworkSig m)

-- * Network operations

-- | Perform a local computation.
run :: m a -> Network m a
run m = toFreer $ Run m

-- | Send a message to a receiver.
send :: (Show a) => a -> [LocTm] -> Network m ()
send a ls = toFreer $ Send a ls

-- | Receive a message from a sender.
recv :: (Read a) => LocTm -> Network m a
recv l = toFreer $ Recv l

-- * Message transport backends

-- | A message transport backend defines a /configuration/ of type @c@ that
-- carries necessary bookkeeping information, then defines @c@ as an instance
-- of `Backend` and provides a `runNetwork` function.
class Backend c where
  runNetwork :: (MonadIO m) => c -> LocTm -> Network m a -> m a
