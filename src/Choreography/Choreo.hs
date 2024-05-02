{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DataKinds #-}

-- | This module defines `Choreo`, the monad for writing choreographies.
module Choreography.Choreo where

import Data.List (delete)
import Control.Monad (void, when)

import Choreography.Location
import Choreography.Network
import Control.Monad.Freer
import GHC.TypeLits
import Logic.Proof (Proof)
import Logic.Classes (refl)
import Logic.Propositional (type (&&), elimAndL, elimAndR, introAnd)

-- * The Choreo monad
-- | A constrained version of `unwrap` that only unwraps values located at a
-- specific location.
type Unwrap (l :: LocTy) = forall ls a. Member l ls -> Located ls a -> a

-- | Effect signature for the `Choreo` monad. @m@ is a monad that represents
-- local computations.
-- TODO take set of participants in Monad

-- type TestTwoLocations (l1 : LocTm) l2 = Set '[l1, l2]

data ChoreoSig (ps :: [LocTy]) m a where
  Local :: (KnownSymbol l)
        => Member l ps
        -> (Unwrap l -> m a)
        -> ChoreoSig ps m (Located '[l] a)

  Comm :: (Show a, Read a, KnownSymbol l, KnownSymbols ls')
       => Proof (IsMember l ls && IsMember l ps)     -- from
       -> Located ls a     -- value
       -> Subset ls' ps    -- to
       -> ChoreoSig ps m (Located ls' a)

  Enclave :: (KnownSymbols ls)
       => Subset ls ps
       -> Choreo ls m b
       -> ChoreoSig ps m (Located ls b)

  Naked :: Subset ps qs
         -> Located qs a
         -> ChoreoSig ps m a

-- |Monad for writing choreographies.
type Choreo ps m = Freer (ChoreoSig ps m)

-- | Run a `Choreo` monad directly.
runChoreo :: Monad m => Choreo ps m a -> m a
runChoreo = interpFreer handler
  where
    handler :: Monad m => ChoreoSig ps m a -> m a
    handler (Local _ m)  = wrap <$> m (unwrap . (@@ nobody))
    handler (Comm l a _) = return $ (wrap . unwrap (elimAndL l @@ nobody)) a
    handler (Enclave _ c) = wrap <$> runChoreo c
    handler (Naked proof a) = return $ unwrap proof a

-- | Endpoint projection.
epp :: Choreo ps m a -> LocTm -> Network m a
epp c l' = interpFreer handler c
  where
    handler :: ChoreoSig ps m a -> Network m a
    handler (Local l m)
      | toLocTm l == l' = wrap <$> run (m $ unwrap . (@@ nobody))
      | otherwise       = return Empty
    handler (Comm s a rs) = do
      let sender = toLocTm $ elimAndR s
      let otherRecipients = sender `delete` toLocs rs
      when (sender == l') $ send (unwrap (elimAndL s @@ nobody) a) otherRecipients
      case () of  -- Is there a better way to write this?
        _ | l' `elem` otherRecipients -> wrap <$> recv sender
          | l' == sender              -> return . wrap . unwrap (elimAndL s @@ nobody) $ a
          | otherwise                 -> return Empty
    handler (Enclave proof ch)
      | l' `elem` toLocs proof = wrap <$> epp ch l'
      | otherwise       = return Empty
    handler (Naked proof a) =  -- Should we have guards here? If `Naked` is safe, then we shouldn't need them...
      return $ unwrap proof a

-- * Choreo operations

-- | Perform a local computation at a given location.
locally :: (KnownSymbol (l :: LocTy))
        => Member l ps           -- ^ Location performing the local computation.
        -> (Unwrap l -> m a) -- ^ The local computation given a constrained
                             -- unwrap funciton.
        -> Choreo ps m (Located '[l] a)
infix 4 `locally`
locally l m = toFreer (Local l m)

-- | Communication between a sender and a receiver.
(~>) :: (Show a, Read a, KnownSymbol l, KnownSymbols ls')
     => (Proof (IsMember l ls && IsMember l ps), Located ls a)  -- ^ A pair of a sender's location and a value located
                          -- at the sender
     -> Subset ls' ps          -- ^ A receiver's location.
     -> Choreo ps m (Located ls' a)
infix 4 ~>
(~>) (l, a) l' = toFreer (Comm l a l')

enclave :: (KnownSymbols ls) => Subset ls ps -> Choreo ls m a -> Choreo ps m (Located ls a)
enclave proof ch = toFreer $ Enclave proof ch

naked :: Subset ps qs
         -> Located qs a
         -> Choreo ps m a
naked proof a = toFreer $ Naked proof a

-- | Conditionally execute choreographies based on a located value.
cond :: (KnownSymbols ls)
     => (Proof (IsSubset ls qs && IsSubset ls ps), Located qs a)
     -> (a -> Choreo ls m b) -- ^ A function that describes the follow-up
                          -- choreographies based on the value of scrutinee.
     -> Choreo ps m (Located ls b)
cond (l, a) c = enclave (elimAndR l) $ naked (elimAndL l) a >>= c

-- | A variant of `~>` that sends the result of a local computation.
(~~>) :: (Show a, Read a, KnownSymbol l, KnownSymbols ls')
      => (Member l ps, Unwrap l -> m a) -- ^ A pair of a sender's location and a local
                                    -- computation.
      -> Subset ls' ps                   -- ^ A receiver's location.
      -> Choreo ps m (Located ls' a)
infix 4 ~~>
(~~>) (l, m) ls' = do
  x <- l `locally` m
  (explicitMember `introAnd` l, x) ~> ls'

broadcastCond :: (Show a, Read a, KnownSymbol l, KnownSymbols ps)
           => (Proof (IsMember l ls && IsMember l ps), Located ls a)
           -> (a -> Choreo ps m b)
           -> Choreo ps m b
broadcastCond (proof, a) c = do a' <- (proof, a) ~> refl
                                b' <- cond (refl `introAnd` refl, a') c
                                naked refl b'

-- | A variant of `cond` that conditonally executes choregraphies based on the
-- result of a local computation.
cond' :: (Show a, Read a, KnownSymbol l, KnownSymbols ps)
      => (Member l ps, Unwrap l -> m a) -- ^ A pair of a location and a local
                                    -- computation.
      -> (a -> Choreo ps m b)          -- ^ A function that describes the follow-up
                                    -- choreographies based on the result of the
                                    -- local computation.
      -> Choreo ps m b
cond' (l, m) c = do
  x <- l `locally` m
  broadcastCond (explicitMember `introAnd` l, x) c

locally_ :: (KnownSymbol l)
        => Member l ps
        -> (Unwrap l -> m ())
        -> Choreo ps m ()
infix 4 `locally_`
locally_ l m = void $ locally l m

_locally :: (KnownSymbol l)
        => Member l ps
        -> m a
        -> Choreo ps m (Located '[l] a)
infix 4 `_locally`
_locally l m = locally l $ const m

_locally_ :: (KnownSymbol l) => Member l ps -> m () -> Choreo ps m ()
infix 4 `_locally_`
_locally_ l m = void $ locally l (const m)

