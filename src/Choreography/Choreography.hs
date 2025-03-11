-- | Operations for writing choreographies.
module Choreography.Choreography where

import Choreography.Core
import Choreography.Locations
import Choreography.Locations.Batteries (ExplicitMember (..))
import GHC.TypeLits

-- * Computation /per se/

-- | Perform a local computation at a given location.
locally ::
  (KnownSymbol (l :: LocTy)) =>
  -- | Location performing the local computation.
  Member l ps ->
  -- | The local computation, which can use a constrained unwrap function.
  (Unwrap l -> m a) ->
  Choreo ps m (Located '[l] a)

infix 4 `locally`

locally l m = conclave (l @@ nobody) $ locally' m

-- | Perform the exact same pure computation in replicate at multiple locations.
--   The computation can not use anything local to an individual party, including their identity.
congruently ::
  forall ls a ps m.
  (KnownSymbols ls) =>
  Subset ls ps ->
  (Unwraps ls -> a) ->
  Choreo ps m (Located ls a)

infix 4 `congruently`

congruently ls a = conclave ls $ congruently' a

-- | Unwrap a value known to the entire census.
naked ::
  (KnownSymbols ps) =>
  Subset ps qs ->
  Located qs a ->
  Choreo ps m a
naked ownership a = congruently' (\un -> un ownership a)

-- * Communication

-- | Writing out the first argument to `~>` can be done a few different ways depending on context, represented by this class.
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

-- | Send a value from one party to the entire census.
broadcast ::
  forall l a ps ls m s.
  (Show a, Read a, KnownSymbol l, KnownSymbols ps, CanSend s l a ls ps) =>
  s ->
  Choreo ps m a
broadcast s = broadcast' (presentToSend s) (ownsMessagePayload s, structMessagePayload s)

-- | Communication between a sender and a list of receivers.
(~>) ::
  (Show a, Read a, KnownSymbol l, KnownSymbols ls', CanSend s l a ls ps) =>
  -- | The message argument can take three forms:
  --
  --   >  (Member sender census, wrapped owners a) -- where sender is explicitly listed in owners
  --
  --   >  (Member sender owners, Subset owners census, wrapped owners a)
  --
  --   >  (Member sender census, (Member sender owners, wrapped owners a)
  s ->
  -- | The recipients.
  Subset ls' ps ->
  Choreo ps m (Located ls' a)

infix 4 ~>

s ~> rs = do
  x :: a <- conclave (presentToSend s @@ rs) $ broadcast' First (ownsMessagePayload s, structMessagePayload s)
  congruently rs (\un -> un consSet x)

-- * Conclaves

-- | Lift a choreography involving fewer parties into the larger party space.
--   This version, where the returned value is Located at the entire conclave, does not add a Located layer.
conclaveToAll :: forall ls a ps m. (KnownSymbols ls) => Subset ls ps -> Choreo ls m (Located ls a) -> Choreo ps m (Located ls a)

infix 4 `conclaveToAll`

conclaveToAll = (`conclaveTo` (refl @ls))

-- | Lift a choreography of involving fewer parties into the larger party space.
--   This version, where the returned value is already Located, does not add a Located layer.
conclaveTo ::
  forall ls a rs ps m.
  (KnownSymbols ls) =>
  Subset ls ps ->
  Subset rs ls ->
  Choreo ls m (Located rs a) ->
  Choreo ps m (Located rs a)

infix 4 `conclaveTo`

conclaveTo subcensus recipients ch = flatten recipients (refl @rs) <$> (subcensus `conclave` ch)
