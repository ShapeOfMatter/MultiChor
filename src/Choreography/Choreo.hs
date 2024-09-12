{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LiberalTypeSynonyms #-}

-- | This module defines `Choreo`, the monad for writing choreographies.
module Choreography.Choreo (
    broadcast
  , cond
  , congruently
  , enclaveTo
  , enclaveToAll
  , fanOut
  , locally
  , locally_
  , _locally
  , _locally_
  , naked
  , parallel
  , parallel_
  , _parallel
  , (~>)
  , (~~>)
  , (-~>)
) where

import Control.Monad (void)
import Data.Functor.Compose (Compose(Compose), getCompose)

import Choreography.Core
import Choreography.Location
import GHC.TypeLits


forLocs :: forall b (ls :: [LocTy]) (ps :: [LocTy]) m.
           (KnownSymbols ls)
        => (forall l. (KnownSymbol l) => Member l ls -> Choreo ps m (b l))
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
s ~> rs = do x :: a <- enclave (presentToSend s @@ rs) $ comm (ownsMessagePayload s, structMessagePayload s)
             congruently rs (\un -> un consSet x)

-- | Conditionally execute choreographies based on a located value. Automatically enclaves.
cond :: (KnownSymbols ls)
     => (Subset ls ps, (Subset ls qs, Located qs a))  -- ^ Tuple: Proof all the parties involved know the branch-guard
                                                                  --and are present, the branch guard
     -> (a -> Choreo ls m b) -- ^ The body of the conditional as a function from the unwrapped value.
     -> Choreo ps m (Located ls b)
cond = uncurry middleware
  where middleware :: (KnownSymbols ls)
             => Subset ls ps
             -> (Subset ls qs, Located qs a)
             -> (a -> Choreo ls m b)
             -> Choreo ps m (Located ls b)
        middleware = getCompose . getCompose <$> ifAnyone (Compose . Compose <$> interior)
        interior ls (owns, a) c = enclave ls $ naked owns a >>= c
--cond (ls, (owns, a)) c = enclave ls $ naked owns a >>= c

naked :: (KnownSymbol p, KnownSymbols ps)
      => Subset (p ': ps) qs
      -> Located qs a
      -> Choreo (p ': ps) m a
naked ownership a = purely (\un -> un ownership a)


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


broadcast :: forall sender val census owners m struct p ps'.
             (Show val, Read val, KnownSymbol sender, KnownSymbols census, CanSend struct sender val owners census, census ~ p ': ps', KnownSymbol p, KnownSymbols ps')
          => struct
          -> Choreo census m val
broadcast s = enclave (presentToSend s @@ allOf @census) (commm (ownsMessagePayload s, payload))
                >>= naked (consSuper $ allOf @census)
  where payload :: Located owners val
        payload = structMessagePayload s
        commm :: (Member sender owners, Located owners val) -> Choreo (sender : census) m val
        commm = comm

congruently :: forall ls a ps m.
               (KnownSymbols ls)
            => Subset ls ps
            -> (Unwraps ls -> a)
            -> Choreo ps m (Located ls a)
infix 4 `congruently`
congruently = getCompose <$> ifAnyone (Compose <$> \ls a -> enclave ls $ purely a)

parallel :: forall ls a ps m.
            (KnownSymbols ls)
         => Subset ls ps
         -> (forall l. (KnownSymbol l) => Member l ls -> Unwrap l -> m a)
         -> Choreo ps m (Faceted ls '[] a)
parallel ls m = forLocs @(Facet a '[]) body ls
  where body :: (KnownSymbol l) => Member l ls -> Choreo ps m (Facet a '[] l)
        body mls = Facet <$> locally (inSuper ls mls) (m mls)

parallel_ :: forall ls ps m.
             (KnownSymbols ls)
          => Subset ls ps
          -> (forall l. (KnownSymbol l) => Member l ls -> Unwrap l -> m ())
          -> Choreo ps m ()
parallel_ ls m = parallel ls m >>= (\(_ :: forall p. Member p ls -> Facet () '[] p) ->
                    return ())   -- WHY CAN"T I JUST USE VOID???!!

_parallel :: forall ls a ps m. (KnownSymbols ls) => Subset ls ps -> m a -> Choreo ps m (Faceted ls '[] a)
_parallel ls m = parallel ls \_ _ -> m

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

ifAnyone :: forall ls f ps a.
            (KnownSymbols ls, Applicative f)
         => (forall l ls'. (ls ~ l ': ls', KnownSymbol l, KnownSymbols ls') => Subset ls ps -> f (Located ls a))
         -> Subset ls ps -> f (Located ls a)
ifAnyone f = case tyUnCons @ls of
  TyNil -> pure . pure $ vacuous
  TyCons -> f

-- | Lift a choreography of involving fewer parties into the larger party space.
--   This version, where the returned value is Located at the entire enclave, takes one less argument than enclaveTo.
enclaveToAll :: forall ls a ps m.
                (KnownSymbols ls)
             => Subset ls ps
             -> Choreo ls m (Located ls a)
             -> Choreo ps m (Located ls a)
infix 4 `enclaveToAll`
enclaveToAll = (`enclaveTo` (allOf @ls))

-- | Lift a choreography of involving fewer parties into the larger party space.
--   This version, where the returned value is already Located, does not add a Located layer.
enclaveTo :: forall ls a rs ps m.
             (KnownSymbols ls)
          => Subset ls ps
          -> Subset rs ls
          -> Choreo ls m (Located rs a)
          -> Choreo ps m (Located rs a)
infix 4 `enclaveTo`
enclaveTo subcensus recipients ch = flatten recipients (allOf @rs) <$> getCompose (ifAnyone (Compose <$> enclave) subcensus) ch


-- | Perform a given choreography for each of several parties, giving each of them a return value that form a new `Faceted`.
fanOut :: (KnownSymbols qs)
       => Subset qs ps  -- ^ The parties to loop over.
       -> (forall q. (KnownSymbol q) => Member q qs -> Choreo ps m (Facet a rs q))  -- ^ The body.
       -> Choreo ps m (Faceted qs rs a)
fanOut qs body = forLocs body qs

{--- | Perform a given choreography for each of several parties; the return values are aggregated as a list located at the recipients.
fanIn :: (KnownSymbols qs, KnownSymbols rs)
       => Subset qs ps  -- ^ The parties who fan in.
       -> Subset rs ps  -- ^ The recipients.
       -> (forall q. (KnownSymbol q) => Member q qs -> Choreo ps m (Located rs a))  -- ^ The body.
       -> Choreo ps m (Located rs [a])
fanIn qs rs body = toFreer $ FanIn qs rs body -}
