-- | This module defines `Choreo`, the monad for writing choreographies,
--   and the closely related `Located` data type.
module Choreography.Core (
    broadcast'
  , Choreo
  -- , ChoreoSig(..)  I can't think of any reasons why we _should_ export this, nor any reason why we shouldn't...
  , congruently'
  , enclave
  , epp
  , flatten
  , locally'
  , Located()
  , othersForget
  , runChoreo
  , Unwrap
  , Unwraps
  , wrap  -- consider renaming or removing.
) where

import Data.List (delete)
import GHC.TypeLits

import Choreography.Locations
import Choreography.Network
import Control.Monad.Freer


-- | A single value known to many parties.
data Located (ls :: [LocTy]) a
  = Wrap a
  | Empty

-- | Wrap a value as a located value.
--   This should be safe to export, while exporting the constuctor would enable pattern matching.
wrap :: a -> Located l a
wrap = Wrap

-- | Unwraps values known to the specified party.
type Unwrap (q :: LocTy) = forall ls a. Member q ls -> Located ls a -> a
-- | Unwraps values known to the specified list of parties.
--   Could be dangerous if the list is empty, but the API is designed so that no value of type `Unwraps '[]` will ever actually get evaluated.
type Unwraps (qs :: [LocTy]) = forall ls a. Subset qs ls -> Located ls a -> a

-- | Unwrap a `Located` value.
--   Unwrapping a empty located value will throw an exception; THIS SHOULD NOT BE EXPORTED!
unwrap :: Unwrap q
unwrap _ (Wrap a) = a
unwrap _ Empty    = error "Located: This should never happen for a well-typed choreography."

-- | Un-nest located values.
flatten :: Subset ls ms -> Subset ls ns -> Located ms (Located ns a) -> Located ls a
infix 3 `flatten`
flatten _ _ Empty = Empty
flatten _ _ (Wrap Empty) = Empty
flatten _ _ (Wrap (Wrap a)) = Wrap a

-- | Cast a `Located` value to a smaller ownership set; useful when working with functions whos arguments have explict ownership sets.
othersForget :: Subset ls owners -> Located owners a -> Located ls a
othersForget _ Empty = Empty
othersForget _ (Wrap a) = Wrap a


data ChoreoSig (ps :: [LocTy]) m a where
  Locally :: (KnownSymbol l)
          => (Unwrap l -> m a)
          -> ChoreoSig '[l] m a

  Congruently :: (KnownSymbols ls)
              => (Unwraps ls -> a)
              -> ChoreoSig ls m a

  Broadcast :: (Show a, Read a, KnownSymbol l)
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
    handler (Locally m) = m unwrap
    handler (Congruently f) = let unwraps :: forall c ls. Subset (p ': ps) ls -> Located ls c -> c
                                  unwraps = unwrap . (\(Subset mx) -> mx First) -- wish i could write this better.
                              in return . f $ unwraps
    handler (Broadcast _ (p, a)) = return $ unwrap p a
    handler (Enclave (_ :: Subset ls (p ': ps)) c) = case tySpine @ls of
      TyNil -> return Empty
      TyCons -> wrap <$> runChoreo c

-- | Endpoint projection.
epp :: forall ps b m. (Monad m, KnownSymbols ps) => Choreo ps m b -> LocTm -> Network m b
epp c l' = interpFreer handler c
  where
    handler :: ChoreoSig ps m a -> Network m a
    handler (Locally m) = run $ m unwrap
    handler (Congruently f) = let unwraps :: forall c ls. Subset ps ls -> Located ls c -> c
                                  unwraps = case tySpine @ps of
                                    TyNil -> error "Undefined projection: the census is empty."
                                    TyCons -> unwrap . (\(Subset mx) -> mx First) -- wish i could write this better.
                              in return . f $ unwraps
    handler (Broadcast s (l, a)) = do
      let sender = toLocTm s
      let otherRecipients = sender `delete` toLocs (refl :: Subset ps ps)
      if sender == l' then do send (unwrap l a) otherRecipients
                              return . unwrap l $ a
                      else recv sender
    handler (Enclave proof ch)
      | l' `elem` toLocs proof = wrap <$> epp ch l'
      | otherwise       = return Empty

-- | Access to the inner "local" monad.
locally' :: (KnownSymbol l)
         => (Unwrap l -> m a)  -- ^ The local action(s), which can use an unwraper function.
         -> Choreo '[l] m a
locally' m = toFreer (Locally m)

-- | Perform the exact same computation in replicate at all participating locations.
--   The computation can not use anything local to an individual party, including their identity.
congruently' :: (KnownSymbols ls)
             => (Unwraps ls -> a)  -- ^ The computation, which can use an unwraper function.
             -> Choreo ls m a
infix 4 `congruently'`
congruently' f = toFreer (Congruently f)

-- | Communicate a value to all present parties.
broadcast' :: (Show a, Read a, KnownSymbol l)
           => Member l ps-- ^ Proof the sender is present
           -> (Member l ls, Located ls a)  -- ^ Proof the sender knows the value, the value.
           -> Choreo ps m a
infix 4 `broadcast'`
broadcast' l a = toFreer (Broadcast l a)

-- | Lift a choreography of involving fewer parties into the larger party space.
--   Adds a `Located ls` layer to the return type.
enclave :: (KnownSymbols ls)
        => Subset ls ps
        -> Choreo ls m a
        -> Choreo ps m (Located ls a)
infix 4 `enclave`
enclave proof ch = toFreer $ Enclave proof ch

