{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE DataKinds #-}

-- | This module defines `Choreo`, the monad for writing choreographies.
module Choreography.Choreo where

import Choreography.Location
import Choreography.Network
import Control.Monad.Freer
import GHC.TypeLits

-- * The Choreo monad
-- | A constrained version of `unwrap` that only unwraps values located at a
-- specific location.
type Unwrap l = forall a. a @ l -> a

-- | Effect signature for the `Choreo` monad. @m@ is a monad that represents
-- local computations.
-- TODO take set of participants in Monad

-- type TestTwoLocations (l1 : LocTm) l2 = Set '[l1, l2]

data ChoreoSig (ps :: [LocTy]) m a where
  Local :: (KnownSymbol l)
        => Member l ps
        -> (Unwrap l -> m a)
        -> ChoreoSig ps m (a @ l)

  Comm :: (Show a, Read a, KnownSymbol l, KnownSymbol l')
       => Member l ps
       -> a @ l
       -> Member l' ps
       -> ChoreoSig ps m (a @ l')

  Cond :: (Show a, Read a, KnownSymbol l)
       => Member l ps
       -> a @ l
       -> (a -> Choreo ps m b)
       -> ChoreoSig ps m b

-- | Monad for writing choreographies.
type Choreo ps m = Freer (ChoreoSig ps m)

-- | Run a `Choreo` monad directly.
runChoreo :: Monad m => Choreo ps m a -> m a
runChoreo = interpFreer handler
  where
    handler :: Monad m => ChoreoSig ps m a -> m a
    handler (Local _ m)  = wrap <$> m unwrap
    handler (Comm _ a _) = return $ (wrap . unwrap) a
    handler (Cond _ a c) = runChoreo $ c (unwrap a)

-- | Endpoint projection.
epp :: Choreo ps m a -> LocTm -> Network m a
epp c l' = interpFreer handler c
  where
    handler :: ChoreoSig ps m a -> Network m a
    handler (Local l m)
      | toLocTm l == l' = wrap <$> run (m unwrap)
      | otherwise       = return Empty
    handler (Comm s a r)
      | toLocTm s == toLocTm r = return $ wrap (unwrap a)
      | toLocTm s == l'        = send (unwrap a) (toLocTm r) >> return Empty
      | toLocTm r == l'        = wrap <$> recv (toLocTm s)
      | otherwise              = return Empty
    handler (Cond l a ch)
      | toLocTm l == l' = broadcast (unwrap a) >> epp (ch (unwrap a)) l'
      | otherwise       = recv (toLocTm l) >>= \x -> epp (ch x) l'

-- * Choreo operations

-- | Perform a local computation at a given location.
locally :: (KnownSymbol l)
        => Member l ps           -- ^ Location performing the local computation.
        -> (Unwrap l -> m a) -- ^ The local computation given a constrained
                             -- unwrap funciton.
        -> Choreo ps m (a @ l)
locally l m = toFreer (Local l m)

-- | Communication between a sender and a receiver.
(~>) :: (Show a, Read a, KnownSymbol l, KnownSymbol l')
     => (Member l ps, a @ l)  -- ^ A pair of a sender's location and a value located
                          -- at the sender
     -> Member l' ps          -- ^ A receiver's location.
     -> Choreo ps m (a @ l')
(~>) (l, a) l' = toFreer (Comm l a l')

-- | Conditionally execute choreographies based on a located value.
cond :: (Show a, Read a, KnownSymbol l)
     => (Member l ps, a @ l)  -- ^ A pair of a location and a scrutinee located on
                          -- it.
     -> (a -> Choreo ps m b) -- ^ A function that describes the follow-up
                          -- choreographies based on the value of scrutinee.
     -> Choreo ps m b
cond (l, a) c = toFreer (Cond l a c)

-- | A variant of `~>` that sends the result of a local computation.
(~~>) :: (Show a, Read a, KnownSymbol l, KnownSymbol l')
      => (Member l ps, Unwrap l -> m a) -- ^ A pair of a sender's location and a local
                                    -- computation.
      -> Member l' ps                   -- ^ A receiver's location.
      -> Choreo ps m (a @ l')
(~~>) (l, m) l' = do
  x <- l `locally` m
  (l, x) ~> l'

-- | A variant of `cond` that conditonally executes choregraphies based on the
-- result of a local computation.
cond' :: (Show a, Read a, KnownSymbol l)
      => (Member l ps, Unwrap l -> m a) -- ^ A pair of a location and a local
                                    -- computation.
      -> (a -> Choreo ps m b)          -- ^ A function that describes the follow-up
                                    -- choreographies based on the result of the
                                    -- local computation.
      -> Choreo ps m b
cond' (l, m) c = do
  x <- l `locally` m
  cond (l, x) c

reveal :: (Show a, Read a, KnownSymbol l)
      => Member l ps
      -> a @ l
      -> Choreo ps m a
reveal l al = cond (l, al) return


locally_ :: (KnownSymbol l)
        => Member l ps           -- ^ Location performing the local computation.
        -> (Unwrap l -> m ()) -- ^ The local computation given a constrained
                             -- unwrap funciton.
        -> Choreo ps m ()
locally_ l m = locally l m >>= const (return ())
