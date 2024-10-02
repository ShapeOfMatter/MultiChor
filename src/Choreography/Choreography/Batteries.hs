module Choreography.Choreography.Batteries where

import Control.Monad (void)

import Choreography.Choreography
import Choreography.Core
import Choreography.Locations
import Choreography.Locations.Batteries((@@))
import GHC.TypeLits


-- * Computation _per se_

-- | Perform a local computation, yielding nothing.
locally_ :: (KnownSymbol l)
        => Member l ps
        -> (Unwrap l-> m ())
        -> Choreo ps m ()
infix 4 `locally_`
locally_ l m = void $ locally l m

-- | Perform a local computation that doesn't need to unwrap any existing `Located` values.
_locally :: (KnownSymbol l)
        => Member l ps
        -> m a
        -> Choreo ps m (Located '[l] a)
infix 4 `_locally`
_locally l m = locally l $ const m

-- | Perform a local computation that doesn't need to unwrap any existing `Located` values and yields nothing.
_locally_ :: (KnownSymbol l) => Member l ps -> m () -> Choreo ps m ()
infix 4 `_locally_`
_locally_ l m = void $ locally l (const m)


-- * Communication

-- | A variant of `~>` that sends the result of a local computation.
(~~>) :: forall a l ls' m ps. (Show a, Read a, KnownSymbol l, KnownSymbols ls')
      => (Member l ps, Unwrap l -> m a) -- ^ A pair of a sender's location and a local computation.
      -> Subset ls' ps                   -- ^ A receiver's location.
      -> Choreo ps m (Located ls' a)
infix 4 ~~>
(~~>) (l, m) ls' = do
  x <- locally l m
  (l, x) ~> ls'

-- | A variant of `~>` that sends the result of a local action that doesn't use existing `Located` variables.
(-~>) :: forall a l ls' m ps. (Show a, Read a, KnownSymbol l, KnownSymbols ls')
      => (Member l ps, m a) -- ^ A pair of a sender's location and a local computation.
      -> Subset ls' ps                   -- ^ A receiver's location.
      -> Choreo ps m (Located ls' a)
infix 4 -~>
(-~>) (l, m) ls' = do
  x <- l `_locally` m
  (l, x) ~> ls'

-- | A variant of `~>` that doesn't use the local monad.
(*~>) :: forall a l ls' m ps. (Show a, Read a, KnownSymbol l, KnownSymbols ls')
      => (Member l ps, Unwrap l -> a) -- ^ A pair of a sender's location and a local computation.
      -> Subset ls' ps                   -- ^ A receiver's location.
      -> Choreo ps m (Located ls' a)
infix 4 *~>
(*~>) (l, m) ls' = do
  x <- l @@ nobody `congruently` \uns -> m $ uns . (@@ nobody)
  (l, x) ~> ls'

-- * Enclaves

-- | Conditionally execute choreographies based on a located value. Automatically enclaves.
cond :: (KnownSymbols ls)
     => (Subset ls ps, (Subset ls qs, Located qs a))  -- ^ Tuple: Proof all the parties involved know the branch-guard
                                                                  --and are present, the branch guard
     -> (a -> Choreo ls m b) -- ^ The body of the conditional as a function from the unwrapped value.
     -> Choreo ps m (Located ls b)
cond (ls, (owns, a)) c = enclave ls $ naked owns a >>= c



