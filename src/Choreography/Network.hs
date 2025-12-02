-- | This module defines the `Network` monad, which represents programs run on
-- individual nodes in a distributed system with explicit sends and receives.
-- To run a `Network` program, we provide a `runNetwork` function that supports
-- multiple message transport backends.
-- Two such backends are provided in "Choreography.Network.Http" and "Choreography.Network.Local",
-- and there should be enough tools here for you to write more as needed.
module Choreography.Network where

import Choreography.Locations (LocTm, Subset)
import Control.Monad.Freer
import Control.Monad.IO.Class

-- * The Network monad

-- | Effect signature for the `Network` monad.
data NetworkSig m ps a where
  -- | Local computation.
  Run ::
    m a ->
    NetworkSig m ps a
  -- | Sending.
  Send ::
    (Show a) =>
    a ->
    [LocTm] ->
    NetworkSig m ps ()
  -- | Receiving.
  Recv ::
    (Read a) =>
    LocTm ->
    NetworkSig m ps a

-- | Monad that represents network programs.
type Network m ps = Freer (NetworkSig m ps)

-- * Network operations

-- | Perform a local computation.
run :: m a -> Network m ps a
run m = toFreer $ Run m

-- | Send a message to a receiver.
send :: (Show a) => a -> [LocTm] -> Network m ps ()
send a ls = toFreer $ Send a ls

-- | Receive a message from a sender.
recv :: (Read a) => LocTm -> Network m ps a
recv l = toFreer $ Recv l

expand :: Subset ps qs -> Network m ps a -> Network m qs a
expand _ = interpFreer $ toFreer . handler
  where
    handler :: NetworkSig m ps a -> NetworkSig m qs a
    handler (Run m) = Run m
    handler (Send a ls) = Send a ls
    handler (Recv l) = Recv l

-- * Message transport backends

-- | A message transport backend defines a /configuration/ of type @c@ that
-- carries necessary bookkeeping information, then defines @c@ as an instance
-- of `Backend` and provides a `runNetwork` function.
class Backend c where
  runNetwork :: (MonadIO m) => c ps -> LocTm -> Network m ps a -> m a
