{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LiberalTypeSynonyms #-}

-- | This module defines `Choreo`, the monad for writing choreographies.
module Choreography.Internal.Choreo where

import Control.Monad (when)
import Data.List (delete)
--import Data.Maybe (catMaybes)
import GHC.TypeLits
--import Logic.Classes (refl, transitive)

import Choreography.Internal.Location
import Choreography.Internal.Network
import Control.Monad.Freer


data ChoreoSig (ps :: [LocTy]) m a where
  Alone :: (KnownSymbol l)
        => (Unwrap l -> m a)
        -> ChoreoSig '[l] m a

  Congruent :: (KnownSymbols ls)
        => (Unwraps ls -> a)
        -> ChoreoSig ls m a

  Comm :: (Show a, Read a, KnownSymbol l)
       => Member l ps     -- from
       -> (Member l ls, Located ls a)     -- value
       -> ChoreoSig ps m a

  Enclave :: (KnownSymbols ls)
       => Subset ls ps
       -> Choreo ls m b
       -> ChoreoSig ps m (Located ls b)

-- | Monad for writing choreographies.
type Choreo ps m = Freer (ChoreoSig ps m)

-- | Run a `Choreo` monad with centralized semantics.
runChoreo :: forall p ps b m. Monad m => Choreo (p ': ps) m b -> m b
runChoreo = interpFreer handler
  where
    handler :: Monad m => ChoreoSig  (p ': ps) m a -> m a
    handler (Alone m) = m unwrap
    handler (Congruent f) = let unwraps :: forall c ls. Subset (p ': ps) ls -> Located ls c -> c
                                unwraps = unwrap . (\(Subset mx) -> mx listedFirst) -- wish i could write this better.
                            in return . f $ unwraps
    handler (Comm _ (p, a)) = return $ unwrap p a
    handler (Enclave (_ :: Subset ls (p ': ps)) c) = case tyUnCons @ls of
      TyNil -> return Empty
      TyCons -> wrap <$> runChoreo c

-- | Endpoint projection.
epp :: forall ps b m. (Monad m, KnownSymbols ps) => Choreo ps m b -> LocTm -> Network m b
epp c l' = interpFreer handler c
  where
    handler :: ChoreoSig ps m a -> Network m a
    handler (Alone m) = run $ m unwrap
    handler (Congruent f) = let unwraps :: forall c ls. Subset ps ls -> Located ls c -> c
                                unwraps = case tyUnCons @ps of
                                  TyNil -> undefined -- you can totally hit this, but maybe it's your own fault?
                                  TyCons -> unwrap . (\(Subset mx) -> mx listedFirst) -- wish i could write this better.
                            in return . f $ unwraps
    handler (Comm s (l, a)) = do
      let sender = toLocTm s
      let otherRecipients = sender `delete` toLocs (refl :: Subset ps ps)
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
congruently :: (KnownSymbols ls)
              => (Unwraps ls -> a)  -- ^ The computation, as a function of the un-wrap-er.
              -> Choreo ls m a
infix 4 `congruently`
congruently f = toFreer (Congruent f)

-- | Communication between a sender and a receiver.
comm :: (Show a, Read a, KnownSymbol l)
     => Member l ps-- ^ Proof the sender is present
     -> (Member l ls, Located ls a)  -- ^ Proof the sender knows the value, the value.
     -> Choreo ps m a
infix 4 `comm`
comm l a = toFreer (Comm l a)

-- | Lift a choreography of involving fewer parties into the larger party space.
--Adds a `Located ls` layer to the return type.
enclave :: (KnownSymbols ls) => Subset ls ps -> Choreo ls m a -> Choreo ps m (Located ls a)
infix 4 `enclave`
enclave proof ch = toFreer $ Enclave proof ch



forLocs :: forall (ls :: [LocTy]) b (ps :: [LocTy]) m.
           (KnownSymbols ls)
        => (forall l. () => Member l ls -> Choreo ps m (b l))
        -> Subset ls ps -- Maybe this can be more general?
        -> Choreo ps m (forall l'. () => Member l' ls -> b l')
forLocs f ls = case tyUnCons @ls of
                 TyCons ->  -- If I put this in do-notation it won't typecheck and I have no idea why.
                     f First >>= (\b ->
                       forLocs (f . Later) (transitive consSet ls)
                       >>= (\fTail ->
                         return \(z :: Member l'' ls) -> case z of
                           First -> b
                           Later lllll -> fTail lllll
                           ))
                 --f h :  (f . inSuper ts) `mapLocs` transitive ts ls
                 TyNil -> return \case {}
