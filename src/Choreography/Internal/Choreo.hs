{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LiberalTypeSynonyms #-}

-- | This module defines `Choreo`, the monad for writing choreographies.
module Choreography.Internal.Choreo where

import Control.Monad (when)
import Data.List (delete)
import GHC.TypeLits

import Choreography.Internal.Location
import Choreography.Internal.Network
import Control.Monad.Freer


data ChoreoSig (ps :: [LocTy]) m a where
  Alone :: (KnownSymbol l)
        => (Unwrap l -> m a)
        -> ChoreoSig '[l] m a

  Purely :: (KnownSymbol l, KnownSymbols ls)
       => (Unwraps (l ': ls) -> a)
       -> ChoreoSig (l ': ls) m a

  Comm :: (Show a, Read a, KnownSymbol l)
       => (Member l ls, Located ls a)
       -> ChoreoSig (l ': ps) m a

  Enclave :: (KnownSymbol l, KnownSymbols ls)
       => Subset (l ': ls) ps
       -> Choreo (l ': ls) m b
       -> ChoreoSig ps m (Located (l ': ls) b)

-- | Monad for writing choreographies.
type Choreo ps m = Freer (ChoreoSig ps m)

-- | Run a `Choreo` monad with centralized semantics.
runChoreo :: forall ps b m. Monad m => Choreo ps m b -> m b
runChoreo = interpFreer handler
  where
    handler :: Monad m => ChoreoSig ps m a -> m a
    handler (Alone m) = m unwrap
    handler (Purely f) = let unwraps :: forall c ls. Subset ps ls -> Located ls c -> c
                             unwraps = unwrap . (\(Subset mx) -> mx First) -- wish i could write this better.
                         in return . f $ unwraps
    handler (Comm (p, a)) = return $ unwrap p a
    handler (Enclave _ c) =  wrap <$> runChoreo c

-- | Endpoint projection.
epp :: forall ps b m. (Monad m, KnownSymbols ps) => Choreo ps m b -> LocTm -> Network m b
epp c l' = interpFreer handler c  -- I think we ought to add some guarentee that l' is actually in ps...
  where
    handler :: ChoreoSig ps m a -> Network m a
    handler (Alone m) = run $ m unwrap
    handler (Purely f) = let unwraps :: forall c ls. Subset ps ls -> Located ls c -> c
                             unwraps = unwrap . (\(Subset mx) -> mx First) -- specifically, shouldn't we be using l' here instead of First?!
                         in return . f $ unwraps
    handler (Comm (l, a)) = do
      let sender = toLocTm (First @ps)
      let otherRecipients = sender `delete` toLocs (refl :: Subset ps ps)  -- we should be able to skip dropping the sender if we do this right...
      when (sender == l') $ send (unwrap l a) otherRecipients
      case () of  -- Is there a better way to write this?
        _ | l' == sender -> return . unwrap l $ a
          | otherwise    -> recv sender
    handler (Enclave proof ch)
      | l' `elem` toLocs proof = wrap <$> epp ch l'
      | otherwise       = return Empty

-- | Access to the inner "local" monad. The parties are not guarenteed to take the same actions, and may use `Faceted`s.
alone :: (KnownSymbol l)
      => (Unwrap l -> m a)  -- ^ The local action(s), as a function of identity and the un-wrap-er.
      -> Choreo '[l] m a
alone m = toFreer (Alone m)

-- | Perform the exact same computation in replicate at multiple locations.
--"Replicate" is stronger than "parallel"; all parties will compute the exact same thing.
--The computation must be pure, and can not use `Faceted`s.
purely :: (KnownSymbol l, KnownSymbols ls)
       => (Unwraps (l ': ls) -> a)  -- ^ The computation, as a function of the un-wrap-er.
       -> Choreo (l ': ls) m a
infix 4 `purely`
purely f = toFreer (Purely f)

-- | Communication between a sender and a receiver.
comm :: (Show a, Read a, KnownSymbol l)
     => (Member l ls, Located ls a)  -- ^ Proof the sender knows the value, the value.
     -> Choreo (l ': ps) m a
infix 4 `comm`
comm a = toFreer (Comm a)

-- | Lift a choreography of involving fewer parties into the larger party space.
--Adds a `Located ls` layer to the return type.
enclave :: (KnownSymbol l, KnownSymbols ls)
        => Subset (l ': ls) ps
        -> Choreo (l ': ls) m a
        -> Choreo ps m (Located (l ': ls) a)
infix 4 `enclave`
enclave proof ch = toFreer $ Enclave proof ch


