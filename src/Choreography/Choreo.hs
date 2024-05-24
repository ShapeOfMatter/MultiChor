{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DataKinds #-}

-- | This module defines `Choreo`, the monad for writing choreographies.
module Choreography.Choreo (
    broadcastCond
  , cond
  , cond'
  , locally
  , locally_
  , _locally
  , _locally_
  , (~>)
  , (~~>)
) where

import Control.Monad (void)

import Choreography.Core
import Choreography.Location
import GHC.TypeLits
import Logic.Proof (Proof)
import Logic.Classes (refl)
import Logic.Propositional (type (&&), elimAndL, elimAndR, introAnd)



-- | Communication between a sender and a receiver.
(~>) :: (Show a, Read a, KnownSymbol l, KnownSymbols ls', Wrapped w)
     => (Proof (IsMember l ls && IsMember l ps), w ls a)  -- ^ Tuple: Proof the sender knows the value and is present, the value.
     -> Subset ls' ps          -- ^ The recipients.
     -> Choreo ps m (Located ls' a)
infix 4 ~>
(~>) = comm

-- | Conditionally execute choreographies based on a located value. Automatically enclaves.
cond :: (KnownSymbols ls)
     => (Proof (IsSubset ls qs && IsSubset ls ps), Located qs a)  -- ^ Tuple: Proof all the parties involved know the branch-guard
                                                                  --and are present, the branch guard
     -> (a -> Choreo ls m b) -- ^ The body of the conditional as a function from the unwrapped value.
     -> Choreo ps m (Located ls b)
cond (l, a) c = enclave (elimAndR l) $ naked (elimAndL l) a >>= c




-- | A variant of `~>` that sends the result of a local computation.
(~~>) :: (Show a, Read a, KnownSymbol l, KnownSymbols ls')
      => (Member l ps, Unwrap l -> m a) -- ^ A pair of a sender's location and a local computation.
      -> Subset ls' ps                   -- ^ A receiver's location.
      -> Choreo ps m (Located ls' a)
infix 4 ~~>
(~~>) (l, m) ls' = do
  x <- l `locally` m
  (explicitMember `introAnd` l, x) ~> ls'

broadcastCond :: (Show a, Read a, KnownSymbol l, KnownSymbols ps, Wrapped w)
           => (Proof (IsMember l ls && IsMember l ps), w ls a)
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

