{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DataKinds #-}

-- | This module defines `Choreo`, the monad for writing choreographies.
module Choreography.Choreo where

import Data.List (delete)
import Data.Maybe (catMaybes)
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
type Unwrap (l :: LocTy) = forall ls a w. (Wrapped w) => Member l ls -> w ls a -> a
type Unwraps (qs :: [LocTy]) = forall ls a. Subset qs ls -> Located ls a -> a

-- | Effect signature for the `Choreo` monad. @m@ is a monad that represents
-- local computations.
-- TODO take set of participants in Monad


data ChoreoSig (ps :: [LocTy]) m a where
  Parallel :: (KnownSymbols ls)
        => Subset ls ps
        -> (forall l. Member l ls -> Unwrap l -> m a)
        -> ChoreoSig ps m (Faceted ls a)

  Replicative :: (KnownSymbols ls)
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

-- |Monad for writing choreographies.
type Choreo ps m = Freer (ChoreoSig ps m)

-- | Run a `Choreo` monad directly.
runChoreo :: forall ps b m. Monad m => Choreo ps m b -> m b
runChoreo = interpFreer handler
  where
    handler :: Monad m => ChoreoSig ps m a -> m a
    handler (Parallel ls m) = let label f l = do z <- f l
                                                 return (toLocTm l, z)
                                  x = label (`m` unwrap') `mapLocs` ls
                              in Faceted <$> sequence x
    handler (Replicative ls f)= case toLocs ls of
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
    handler (Replicative ls f)
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

-- * Choreo operations

parallel :: (KnownSymbols ls)
         => Subset ls ps
         -> (forall l. Member l ls -> Unwrap l -> m a)
         -> Choreo ps m (Faceted ls a)
parallel ls m = toFreer (Parallel ls m)

replicatively :: (KnownSymbols ls)
              => Subset ls ps
              -> (Unwraps ls -> a)
              -> Choreo ps m (Located ls a)
infix 4 `replicatively`
replicatively ls f = toFreer (Replicative ls f)

-- | Communication between a sender and a receiver.
(~>) :: (Show a, Read a, KnownSymbol l, KnownSymbols ls', Wrapped w)
     => (Proof (IsMember l ls && IsMember l ps), w ls a)  -- ^ A pair of a sender's location and a value located
                          -- at the sender
     -> Subset ls' ps          -- ^ A receiver's location.
     -> Choreo ps m (Located ls' a)
infix 4 ~>
(~>) (l, a) l' = toFreer (Comm l a l')

enclave :: (KnownSymbols ls) => Subset ls ps -> Choreo ls m a -> Choreo ps m (Located ls a)
infix 4 `enclave`
enclave proof ch = toFreer $ Enclave proof ch

naked :: Subset ps qs
         -> Located qs a
         -> Choreo ps m a
infix 4 `naked`
naked proof a = toFreer $ Naked proof a

-- | Conditionally execute choreographies based on a located value.
cond :: (KnownSymbols ls)
     => (Proof (IsSubset ls qs && IsSubset ls ps), Located qs a)
     -> (a -> Choreo ls m b) -- ^ A function that describes the follow-up
                          -- choreographies based on the value of scrutinee.
     -> Choreo ps m (Located ls b)
cond (l, a) c = enclave (elimAndR l) $ naked (elimAndL l) a >>= c

fanOut :: (KnownSymbols qs, Wrapped w)
       => Subset qs ps
       -> (forall q. (KnownSymbol q) => Member q qs -> Choreo ps m (w '[q] a))
       -> Choreo ps m (Faceted qs a)
fanOut qs body = toFreer $ FanOut qs body

fanIn :: (KnownSymbols qs, KnownSymbols rs)
       => Subset qs ps
       -> Subset rs ps
       -> (forall q. (KnownSymbol q) => Member q qs -> Choreo ps m (Located rs a))
       -> Choreo ps m (Located rs [a])
fanIn qs rs body = toFreer $ FanIn qs rs body



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

-- | Perform a local computation at a given location.
locally :: (KnownSymbol (l :: LocTy))
        => Member l ps           -- ^ Location performing the local computation.
        -> (Unwrap l -> m a) -- ^ The local computation given a constrained
                             -- unwrap funciton.
        -> Choreo ps m (Located '[l] a)
infix 4 `locally`
locally l m = localize explicitMember <$> parallel (l @@ nobody) (\l' un -> m (\lLS -> un (inSuper (consSub explicitSubset lLS) l')))

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

