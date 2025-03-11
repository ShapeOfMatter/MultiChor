-- | A zoo of helpful derived functions for writing choreographies.
module Choreography.Choreography.Batteries where

import Choreography.Choreography
import Choreography.Core
import Choreography.Locations
import Control.Monad (void)
import GHC.TypeLits

-- * Computation /per se/

-- | Perform a local computation, yielding nothing.
locally_ ::
  (KnownSymbol l) =>
  Member l ps ->
  (Unwrap l -> m ()) ->
  Choreo ps m ()

infix 4 `locally_`

locally_ l m = void $ locally l m

-- | Perform a local computation that doesn't need to unwrap any existing `Located` values.
_locally ::
  (KnownSymbol l) =>
  Member l ps ->
  m a ->
  Choreo ps m (Located '[l] a)

infix 4 `_locally`

_locally l m = locally l $ const m

-- | Perform a local computation that doesn't need to unwrap any existing `Located` values and yields nothing.
_locally_ :: (KnownSymbol l) => Member l ps -> m () -> Choreo ps m ()

infix 4 `_locally_`

_locally_ l m = void $ locally l (const m)

-- | Perform a pure computation at a single location.
purely ::
  forall l a ps m.
  (KnownSymbol l) =>
  Member l ps ->
  (Unwrap l -> a) ->
  Choreo ps m (Located '[l] a)

infix 4 `purely`

purely l a =
  congruently
    (Subset \First -> l)
    ( \un ->
        let un' :: Unwrap l -- There is definitley a nicer way to write this...
            un' mem = un (Subset \First -> mem)
         in a un'
    )

-- * Communication

-- | A variant of `~>` that sends the result of a local computation.
(~~>) ::
  forall a l ls' m ps.
  (Show a, Read a, KnownSymbol l, KnownSymbols ls') =>
  -- | A pair of a sender's location and a local computation.
  (Member l ps, Unwrap l -> m a) ->
  -- | A receiver's location.
  Subset ls' ps ->
  Choreo ps m (Located ls' a)

infix 4 ~~>

(~~>) (l, m) ls' = do
  x <- locally l m
  (l, x) ~> ls'

-- | A variant of `~>` that sends the result of a local action that doesn't use existing `Located` variables.
(-~>) ::
  forall a l ls' m ps.
  (Show a, Read a, KnownSymbol l, KnownSymbols ls') =>
  -- | A pair of a sender's location and a local computation.
  (Member l ps, m a) ->
  -- | A receiver's location.
  Subset ls' ps ->
  Choreo ps m (Located ls' a)

infix 4 -~>

(-~>) (l, m) ls' = do
  x <- l `_locally` m
  (l, x) ~> ls'

-- | A variant of `~>` that doesn't use the local monad.
(*~>) ::
  forall a l ls' m ps.
  (Show a, Read a, KnownSymbol l, KnownSymbols ls') =>
  -- | A pair of a sender's location and a local computation.
  (Member l ps, Unwrap l -> a) ->
  -- | A receiver's location.
  Subset ls' ps ->
  Choreo ps m (Located ls' a)

infix 4 *~>

(*~>) (l, m) ls' = do
  x <- l @@ nobody `congruently` \uns -> m $ uns . (@@ nobody)
  (l, x) ~> ls'

-- * Conclaves

-- | Conditionally execute choreographies based on a located value. Automatically conclaves.
cond ::
  (KnownSymbols ls) =>
  -- | Tuple: Proof all the parties involved know the branch-guard
  -- and are present, the branch guard
  (Subset ls ps, (Subset ls qs, Located qs a)) ->
  -- | The body of the conditional as a function from the unwrapped value.
  (a -> Choreo ls m b) ->
  Choreo ps m (Located ls b)
cond (ls, (owns, a)) c = conclave ls $ naked owns a >>= c
