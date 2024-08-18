{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DataKinds #-}

-- | This module defines `Choreo`, the monad for writing choreographies.
module Choreography.Choreo (
    broadcast
  , cond
  , enclaveTo
  , enclaveToAll
  , locally
  , locally_
  , _locally
  , _locally_
  --, parallel_
  --, _parallel
  , (~>)
  , (~~>)
  , (-~>)
) where

import Control.Monad (void)

import Choreography.Core
import Choreography.Location
import GHC.TypeLits


--class CanSend loc val owners census struct | struct -> loc val owners census where
  --normalSendArgs :: struct -> (Member loc census, Member loc owners, val)
class (KnownSymbol loc) => CanSend struct loc val owners census | struct -> loc val owners census where
  presentToSend :: struct -> Member loc census
  ownsMessagePayload :: struct -> Member loc owners
  structMessagePayload :: struct -> Located owners val

instance (KnownSymbol l) => CanSend (Member l ps, (Member l ls, Located ls a)) l a ls ps where
  presentToSend = fst
  ownsMessagePayload = fst . snd
  structMessagePayload = snd . snd

instance (KnownSymbol l, ExplicitMember l ls) => CanSend (Member l ps, Located ls a) l a ls ps where
  presentToSend = fst
  ownsMessagePayload = const explicitMember
  structMessagePayload = snd

instance (KnownSymbol l) => CanSend (Member l ls, Subset ls ps, Located ls a) l a ls ps where
  presentToSend (m, s, _) = inSuper s m
  ownsMessagePayload (m, _, _) = m
  structMessagePayload (_, _, p) = p

-- | Communication between a sender and a receiver.
(~>) :: (Show a, Read a, KnownSymbol l, KnownSymbols ls', CanSend s l a ls ps)
     => s  -- ^ The message argument can take three forms:
           --     `(Member sender census, wrapped owners a)` where the sender is explicitly listed in owners,
           --     `(Member sender owners, Subset owners census, wrapped owners a)`, or
           --     `(Member sender census, (Member sender owners, wrapped owners a)`.
     -> Subset ls' ps          -- ^ The recipients.
     -> Choreo ps m (Located ls' a)
infix 4 ~>
s ~> rs = do x :: a <- enclave (presentToSend s @@ rs) $ comm listedFirst (ownsMessagePayload s, structMessagePayload s)
             enclave rs $ congruently (\un -> un consSet x)

-- | Conditionally execute choreographies based on a located value. Automatically enclaves.
cond :: (KnownSymbols ls)
     => (Subset ls ps, (Subset ls qs, Located qs a))  -- ^ Tuple: Proof all the parties involved know the branch-guard
                                                                  --and are present, the branch guard
     -> (a -> Choreo ls m b) -- ^ The body of the conditional as a function from the unwrapped value.
     -> Choreo ps m (Located ls b)
cond (ls, (owns, a)) c = enclave ls $ naked owns a >>= c

naked :: (KnownSymbols ps)
      => Subset ps qs
      -> Located qs a
      -> Choreo ps m a
naked ownership a = congruently (\un -> un ownership a)


-- | A variant of `~>` that sends the result of a local computation.
(~~>) :: forall a l ls' m ps. (Show a, Read a, KnownSymbol l, KnownSymbols ls')
      => (Member l ps, Unwrap l -> m a) -- ^ A pair of a sender's location and a local computation.
      -> Subset ls' ps                   -- ^ A receiver's location.
      -> Choreo ps m (Located ls' a)
infix 4 ~~>
(~~>) (l, m) ls' = do
  x <- locally l m
  (l, x) ~> ls'

-- | A variant of `~>` that sends the result of a local action that doesn't use existing bound variables.
(-~>) :: forall a l ls' m ps. (Show a, Read a, KnownSymbol l, KnownSymbols ls')
      => (Member l ps, m a) -- ^ A pair of a sender's location and a local computation.
      -> Subset ls' ps                   -- ^ A receiver's location.
      -> Choreo ps m (Located ls' a)
infix 4 -~>
(-~>) (l, m) ls' = do
  x <- l `_locally` m
  (l, x) ~> ls'


broadcast :: forall l a ps ls m s.
             (Show a, Read a, KnownSymbol l, KnownSymbols ps, CanSend s l a ls ps)
          => s
          -> Choreo ps m a
broadcast s = comm (presentToSend s) (ownsMessagePayload s, structMessagePayload s)

{-
parallel_ :: forall ls ps m.
             (KnownSymbols ls)
          => Subset ls ps
          -> (forall l. (KnownSymbol l) => Member l ls -> Unwrap l ->m ())
          -> Choreo ps m ()
parallel_ ls m = void $ parallel ls m

_parallel :: forall ls a ps m. (KnownSymbols ls) => Subset ls ps -> m a -> Choreo ps m (Faceted ls a)
_parallel ls m = parallel ls \_ _ -> m
-}

-- | Perform a local computation at a given location.
locally :: (KnownSymbol (l :: LocTy))
        => Member l ps           -- ^ Location performing the local computation.
        -> (Unwrap l -> m a) -- ^ The local computation given a constrained
                             -- unwrap funciton.
        -> Choreo ps m (Located '[l] a)
infix 4 `locally`
locally l m = enclave (l @@ nobody) $ alone m
locally_ :: (KnownSymbol l)
        => Member l ps
        -> (Unwrap l-> m ())
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
enclaveTo subcensus recipients ch = flatten recipients (allOf @rs) <$> (subcensus `enclave` ch)

