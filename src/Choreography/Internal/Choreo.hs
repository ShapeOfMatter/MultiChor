{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DataKinds #-}

-- | This module defines `Choreo`, the monad for writing choreographies.
module Choreography.Internal.Choreo where

import Control.Monad (when)
import Data.List (delete)
import Data.Maybe (catMaybes)
import GHC.TypeLits
import Logic.Classes (refl)
import Logic.Proof (Proof)
import Logic.Propositional (type (&&), elimAndL, elimAndR)

import Choreography.Internal.Location
import Choreography.Network
import Control.Monad.Freer

-- | Unwraps values known to the specified party.
type Unwrap (l :: LocTy) = forall ls a w. (Wrapped w) => Member l ls -> w ls a -> a
-- | Unwraps `Located` values known to the specified party.
type Unwraps (qs :: [LocTy]) = forall ls a. Subset qs ls -> Located ls a -> a


data ChoreoSig (ps :: [LocTy]) m a where
  Parallel :: (KnownSymbols ls)
        => Subset ls ps
        -> (forall l. (KnownSymbol l) => Member l ls -> Unwrap l -> m a)
        -> ChoreoSig ps m (Faceted ls a)

  Congruent :: (KnownSymbols ls)
        => Subset ls ps
        -> (Unwraps ls -> a)
        -> ChoreoSig ps m (Located ls a)

  Comm :: (Show a, Read a, KnownSymbol l, KnownSymbols ls', Wrapped w)
       => Proof (IsMember l ls && IsMember l ps)     -- from
       -> w ls a     -- value
       -> Subset ls' ps    -- to
       -> ChoreoSig ps m (Located ls' a)

  Enclave :: (KnownSymbols ls)
       => Subset ls ps
       -> Choreo ls m b
       -> ChoreoSig ps m (Located ls b)

  Naked :: Subset ps qs
         -> Located qs a
         -> ChoreoSig ps m a

  FanOut :: (KnownSymbols qs, Wrapped w)
       => Subset qs ps
       -> (forall q. (KnownSymbol q) => Member q qs -> Choreo ps m (w '[q] a))
       -> ChoreoSig ps m (Faceted qs a)

  FanIn :: (KnownSymbols qs, KnownSymbols rs)
       => Subset qs ps
       -> Subset rs ps
       -> (forall q. (KnownSymbol q) => Member q qs -> Choreo ps m (Located rs a))
       -> ChoreoSig ps m (Located rs [a])

-- | Monad for writing choreographies.
type Choreo ps m = Freer (ChoreoSig ps m)

-- | Run a `Choreo` monad with centralized semantics.
runChoreo :: forall ps b m. Monad m => Choreo ps m b -> m b
runChoreo = interpFreer handler
  where
    handler :: Monad m => ChoreoSig ps m a -> m a
    handler (Parallel ls m) = let label f l = do z <- f l
                                                 return (toLocTm l, z)
                                  x = label (`m` unwrap') `mapLocs` ls
                              in Faceted <$> sequence x
    handler (Congruent ls f)= case toLocs ls of
      [] -> return Empty  -- I'm not 100% sure we should care about this situation...
      _  -> return . wrap . f $ unwrap
    handler (Comm l a _) = return $ (wrap . unwrap' (elimAndL l)) a
    handler (Enclave _ c) = wrap <$> runChoreo c
    handler (Naked proof a) = return $ unwrap proof a
    handler (FanOut (qs :: Subset qs ps) (body :: forall q. (KnownSymbol q) => Member q qs -> Choreo ps m (w '[q] a))) =
      let body' :: forall q. (KnownSymbol q) => Member q qs -> m (LocTm, a)
          body' q = (toLocTm q, ) . unwrap' (explicitMember :: Member q '[q]) <$> runChoreo (body q)
          bs = body' `mapLocs` qs
      in Faceted <$> sequence bs
    handler (FanIn (qs :: Subset qs ps) (rs :: Subset rs ps) (body :: forall q. (KnownSymbol q) => Member q qs -> Choreo ps m (Located rs a))) =
      let body' :: forall q. (KnownSymbol q) => Member q qs -> m a
          body' q = unwrap (refl :: Subset rs rs) <$> runChoreo (body q)
          bs = body' `mapLocs` qs
      in case toLocs rs of
        [] -> return Empty
        _ -> Wrap <$> sequence bs

-- | Endpoint projection.
epp :: (Monad m) => Choreo ps m a -> LocTm -> Network m a
epp c l' = interpFreer handler c
  where
    handler :: (Monad m) => ChoreoSig ps m a -> Network m a
    handler (Parallel ls m) = (Faceted <$>) . sequence . catMaybes $ (
        \l -> if toLocTm l == l' then Just $ (l', ) <$> run (m l unwrap') else Nothing
      ) `mapLocs` ls
    handler (Congruent ls f)
      | l' `elem` toLocs ls = return . wrap . f $ unwrap
      | otherwise = return Empty
    handler (Comm s a rs) = do
      let sender = toLocTm $ elimAndR s
      let otherRecipients = sender `delete` toLocs rs
      when (sender == l') $ send (unwrap' (elimAndL s) a) otherRecipients
      case () of  -- Is there a better way to write this?
        _ | l' `elem` otherRecipients -> wrap <$> recv sender
          | l' == sender              -> return . wrap . unwrap' (elimAndL s) $ a
          | otherwise                 -> return Empty
    handler (Enclave proof ch)
      | l' `elem` toLocs proof = wrap <$> epp ch l'
      | otherwise       = return Empty
    handler (Naked proof a) =  -- Should we have guards here? If `Naked` is safe, then we shouldn't need them...
      return $ unwrap proof a
    handler (FanOut (qs :: Subset qs ps) (body :: forall q. (KnownSymbol q) => Member q qs -> Choreo ps m (w '[q] a))) =
      let body' :: forall q. (KnownSymbol q) => Member q qs -> Network m (LocTm, Maybe a)
          body' q = (toLocTm q, ) . safeUnwrap (explicitMember :: Member q '[q]) <$> epp (body q) l'
          safeUnwrap :: forall q. (KnownSymbol q) => Member q '[q] -> w '[q] a -> Maybe a
          safeUnwrap q = if toLocTm q == l' then Just <$> unwrap' q else const Nothing
          bs = body' `mapLocs` qs
          filterOwned :: [(LocTm, Maybe a)] -> [(LocTm, a)]
          filterOwned lmas = [(l, a) | (l, Just a) <- lmas]  -- pretty sure non-matching Nothings just get filtered out?
      in Faceted . filterOwned <$> sequence bs
    handler (FanIn (qs :: Subset qs ps) (rs :: Subset rs ps) (body :: forall q. (KnownSymbol q) => Member q qs -> Choreo ps m (Located rs a))) =
      let bs = body `mapLocs` qs
      in do las :: [Located rs a] <- epp (sequence bs) l'
            return if l' `elem` toLocs rs then Wrap $ unwrap (refl :: Subset rs rs) <$> las else Empty

-- | Access to the inner "local" monad. The parties are not guarenteed to take the same actions, and may use `Faceted`s.
parallel :: (KnownSymbols ls)
         => Subset ls ps  -- ^ A set of parties who will all perform the action(s) in parallel.
         -> (forall l. (KnownSymbol l) => Member l ls -> Unwrap l -> m a)  -- ^ The local action(s), as a function of identity and the un-wrap-er.
         -> Choreo ps m (Faceted ls a)
parallel ls m = toFreer (Parallel ls m)

-- | Perform the exact same computation in replicate at multiple locations.
--"Replicate" is stronger than "parallel"; all parties will compute the exact same thing.
--The computation must be pure, and can not use `Faceted`s.
congruently :: (KnownSymbols ls)
              => Subset ls ps  -- ^ The set of parties who will perform the computation.
              -> (Unwraps ls -> a)  -- ^ The computation, as a function of the un-wrap-er.
              -> Choreo ps m (Located ls a)
infix 4 `congruently`
congruently ls f = toFreer (Congruent ls f)

-- | Communication between a sender and a receiver.
comm :: (Show a, Read a, KnownSymbol l, KnownSymbols ls', Wrapped w)
     => (Proof (IsMember l ls && IsMember l ps), w ls a)  -- ^ Tuple: Proof the sender knows the value and is present, the value.
     -> Subset ls' ps          -- ^ The recipients.
     -> Choreo ps m (Located ls' a)
infix 4 `comm`
comm (l, a) l' = toFreer (Comm l a l')

-- | Lift a choreography of involving fewer parties into the larger party space.
--Adds a `Located ls` layer to the return type.
enclave :: (KnownSymbols ls) => Subset ls ps -> Choreo ls m a -> Choreo ps m (Located ls a)
infix 4 `enclave`
enclave proof ch = toFreer $ Enclave proof ch

-- | Un-locates a value known to everyone present in the choreography.
naked :: Subset ps qs -- ^ Proof that everyone knows it.
         -> Located qs a  -- ^ The value.
         -> Choreo ps m a
infix 4 `naked`
naked proof a = toFreer $ Naked proof a

-- | Perform a given choreography for each of several parties, giving each of them a return value that form a new `Faceted`.
fanOut :: (KnownSymbols qs, Wrapped w)
       => Subset qs ps  -- ^ The parties to loop over.
       -> (forall q. (KnownSymbol q) => Member q qs -> Choreo ps m (w '[q] a))  -- ^ The body.
       -> Choreo ps m (Faceted qs a)
fanOut qs body = toFreer $ FanOut qs body

-- | Perform a given choreography for each of several parties; the return values are aggregated as a list located at the recipients.
fanIn :: (KnownSymbols qs, KnownSymbols rs)
       => Subset qs ps  -- ^ The parties who fan in.
       -> Subset rs ps  -- ^ The recipients.
       -> (forall q. (KnownSymbol q) => Member q qs -> Choreo ps m (Located rs a))  -- ^ The body.
       -> Choreo ps m (Located rs [a])
fanIn qs rs body = toFreer $ FanIn qs rs body

