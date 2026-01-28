-- | Types, functions, and structures for writing choreographies with variable numbers of participants.
module Choreography.Polymorphism where

import Choreography.Choreography
import Choreography.Choreography.Batteries ((*~>))
import Choreography.Core
import Choreography.Network
import Control.Monad (void)
import Data.Functor.Compose (Compose (Compose))
import Data.Functor.Const (Const (Const, getConst))
import Data.Known.Knowable
import Data.Known.Membership
import Data.Known.TypeIndexed



-- * Non-congruent parallel located values

-- | A unified representation of possibly-distinct homogeneous values owned by many parties.
type Faceted parties common a = TIndexed parties (Facet a common)

-- | Repackages `Located` with the type arguments correctly arranged for use with t`TIndexed`.
newtype Facet a common p = Facet {getFacet :: Located (p ': common) a}

-- | Get a `Located` value of a `Faceted` at a given location.
localize :: (Known Party l) => Member l ls -> Faceted ls common a -> Located (l ': common) a
localize l (TIndexed f) = getFacet $ f l

-- | In a context where unwrapping located values is possible, get the respective value stored in a `Faceted`.
viewFacet :: (Known Party l) => Unwrap l -> Member l ls -> Faceted ls common a -> a
viewFacet un l = un First . localize l


-- * #choreographicfunctions# Choreographic functions

-- | Perform a local computation at all of a list of parties, yielding a `Faceted`.
parallel ::
  forall ls a ps m.
  (Known [Party] ls) =>
  -- | The parties who will do the computation must be present in the census.
  Subset ls ps ->
  -- | The local computation has access to the identity of the party in question,
  --   in additon to the usual unwrapper function.
  (forall l. (Known Party l) => Member l ls -> Unwrap l -> m a) -> -- Could promote this to TIndexed too, but ergonomics might be worse?
  Choreo ps m (Faceted ls '[] a)
parallel ls m = fanOut \mls -> locally (inSuper ls mls) (m mls)

-- | Perform a local computation at all of a list of parties, yielding nothing.
parallel_ ::
  forall ls ps m.
  (Known [Party] ls) =>
  Subset ls ps ->
  (forall l. (Known Party l) => Member l ls -> Unwrap l -> m ()) ->
  Choreo ps m ()
parallel_ ls m = void $ parallel ls m

-- | Perform a local computation, that doesn't use any existing `Located` values and doesn't depend on the respective party's identity,
--   at all of a list of parties, yielding a `Faceted`.
_parallel :: forall ls a ps m. (Known [Party] ls) => Subset ls ps -> m a -> Choreo ps m (Faceted ls '[] a)
_parallel ls m = parallel ls \_ _ -> m

-- | Perform a given choreography for each of several parties, giving each of them a return value that form a new `Faceted`.
fanOut ::
  (Known [Party] qs) =>
  -- | The body.  -- kinda sketchy that rs might not be a subset of ps...
  (forall q. (Known Party q) => Member q qs -> Choreo ps m (Located (q ': rs) a)) ->
  Choreo ps m (Faceted qs rs a)
fanOut body = sequenceT (TIndexed $ Compose . (Facet <$>) <$> body)

-- | Perform a given choreography for each of several parties; the return values are known to recipients but not necessarily to the loop-parties.
fanIn ::
  (Known [Party] qs, Known [Party] rs) =>
  -- | The recipients.
  Subset rs ps ->
  -- | The body.
  (forall q. (Known Party q) => Member q qs -> Choreo ps m (Located rs a)) ->
  Choreo ps m (Located rs (TVec qs a))
fanIn rs body = do
  (TIndexed x) <- sequenceT (TIndexed $ Compose . (Const <$>) <$> body)
  rs `congruently` \un -> pack $ \q -> un refl (getConst $ x q)

-- | The owner of a t`TVec` sends its elements to their respective parties, resulting in a `Faceted`.
--   This represents the "scatter" idea common in parallel computing contexts.
scatter ::
  forall census sender recipients a m.
  (Known Party sender, Known [Party] recipients, Show a, Read a) =>
  Member sender census ->
  Subset recipients census ->
  Located '[sender] (TVec recipients a) ->
  Choreo census m (Faceted recipients '[sender] a)
scatter sender recipients values = fanOut \r ->
  (sender, \un -> un First values ! r) *~> inSuper recipients r @@ sender @@ nobody

-- | The many owners of a `Faceted` each send their respective values to a constant list of recipients, resulting in a t`TVec`.
--   This represents the "gather" idea common in parallel computing contexts.
gather ::
  forall census recipients senders a dontcare m.
  (Known [Party] senders, Known [Party] recipients, Show a, Read a) =>
  Subset senders census ->
  Subset recipients census ->
  Faceted senders dontcare a ->
  Choreo census m (Located recipients (TVec senders a)) -- could be Faceted senders recipients instead...
gather senders recipients (TIndexed values) = fanIn recipients \s ->
  (inSuper senders s, getFacet $ values s) ~> recipients
