{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DataKinds #-}

-- | This module defines `Choreo`, the monad for writing choreographies.
module Choreography.Choreo (
    broadcastCond
  , cond
  , cond'
  , enclaveTo
  , enclaveToAll
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
import Logic.Propositional (type (&&), elimAndL, elimAndR, introAnd)


--class CanSend loc val owners census struct | struct -> loc val owners census where
  --normalSendArgs :: struct -> (Member loc census, Member loc owners, val)
class (Wrapped w, KnownSymbol loc) => CanSend struct loc val owners census w | struct -> loc val owners census w where
  presentToSend :: struct -> Member loc census
  ownsMessagePayload :: struct -> Member loc owners
  structMessagePayload :: struct -> w owners val

instance (Wrapped w, KnownSymbol l) => CanSend (Member l ps, (Member l ls, w ls a)) l a ls ps w where
  presentToSend = fst
  ownsMessagePayload = fst . snd
  structMessagePayload = snd . snd

instance (Wrapped w, KnownSymbol l, ExplicitMember l ls) => CanSend (Member l ps, w ls a) l a ls ps w where
  presentToSend = fst
  ownsMessagePayload = const explicitMember
  structMessagePayload = snd

instance (Wrapped w, KnownSymbol l) => CanSend (Member l ls, Subset ls ps, w ls a) l a ls ps w where
  presentToSend (m, s, _) = inSuper s m
  ownsMessagePayload (m, _, _) = m
  structMessagePayload (_, _, p) = p

-- | Communication between a sender and a receiver.
(~>) :: (Show a, Read a, KnownSymbol l, KnownSymbols ls', CanSend s l a ls ps w)
     => s  -- ^ The message argument can take three forms:
           --     `(Member sender census, wrapped owners a)` where the sender is explicitly listed in owners,
           --     `(Member sender owners, Subset owners census, wrapped owners a)`, or
           --     `(Member sender census, (Member sender owners, wrapped owners a)`.
     -> Subset ls' ps          -- ^ The recipients.
     -> Choreo ps m (Located ls' a)
infix 4 ~>
s ~> rs = comm (presentToSend s) (ownsMessagePayload s, structMessagePayload s) rs

-- | Conditionally execute choreographies based on a located value. Automatically enclaves.
cond :: (KnownSymbols ls)
     => (Proof (IsSubset ls qs && IsSubset ls ps), Located qs a)  -- ^ Tuple: Proof all the parties involved know the branch-guard
                                                                  --and are present, the branch guard
     -> (a -> Choreo ls m b) -- ^ The body of the conditional as a function from the unwrapped value.
     -> Choreo ps m (Located ls b)
cond (l, a) c = enclave (elimAndR l) $ naked (elimAndL l) a >>= c




-- | A variant of `~>` that sends the result of a local computation.
(~~>) :: forall a l ls' m ps. (Show a, Read a, KnownSymbol l, KnownSymbols ls')
      => (Member l ps, Unwrap l -> m a) -- ^ A pair of a sender's location and a local computation.
      -> Subset ls' ps                   -- ^ A receiver's location.
      -> Choreo ps m (Located ls' a)
infix 4 ~~>
(~~>) (l, m) ls' = do
  x <- l `locally` m
  (l, x) ~> ls'

broadcastCond :: forall l ls a b w ps m.
              (Show a, Read a, KnownSymbol l, KnownSymbols ps, Wrapped w)
           => (Proof (IsMember l ls && IsMember l ps), w ls a)
           -> (a -> Choreo ps m b)
           -> Choreo ps m b
-- broadcastCond (proof, a) c = do a' <- (proof, a) ~> refl
-- Hmm should I change broadcastCond too? I'm guessing we want to keep it.
broadcastCond (proof, a) c = do a' <- (elimAndR proof, (elimAndL proof, a)) ~> allOf @ps
                                b' <- cond (allOf `introAnd` allOf, a') c
                                naked allOf b'

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

-- | Lift a choreography of involving fewer parties into the larger party space.
--   This version, where the returned value is Located at the entire enclave, does not add a Located layer.
enclaveToAll :: forall ls a ps m. (KnownSymbols ls) => Subset ls ps -> Choreo ls m (Located ls a) -> Choreo ps m (Located ls a)
infix 4 `enclaveToAll`
enclaveToAll = (`enclaveTo` (allOf @ls))

-- | Lift a choreography of involving fewer parties into the larger party space.
--   This version, where the returned value is Located at the entire enclave, does not add a Located layer.
enclaveTo :: forall ls a rs ps m.
             (KnownSymbols ls)
          => Subset ls ps
          -> Subset rs ls
          -> Choreo ls m (Located rs a)
          -> Choreo ps m (Located rs a)
infix 4 `enclaveTo`
enclaveTo subcensus recipients ch = flatten (recipients `introAnd` (allOf @rs)) <$> (subcensus `enclave` ch)

