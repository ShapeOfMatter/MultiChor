{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

{-
# This example was carried over from earlier work, likely HasChor. It doesn't yet have a unit test
# attached to it.

# Example: Quicksort

## Overview

This example implements the three-way concurrent implementation of quicksort.

## Execution

This choreography will sort the list `[1, 6, 5, 3, 4, 2, 7, 8]`. It uses the local backend and
distribute the computation over threads.

```bash
cabal run quicksort
[1,2,3,4,5,6,7,8]
```

`Reference.hs` contains a single-threaded reference implementation of the algorithm.
-}

module QuickSort where

import Choreography
import CLI
import Data (unsafeQuietHead)
import Data.Known.Knowable
import Data.Known.Membership
import EasyMain (easyMain)
import GHC.TypeLits (KnownSymbol)

type Participants = ["primary", "worker1", "worker2"]
primary :: forall ps. ExplicitMember "primary" ps => Member "primary" ps
primary = explicitMember
worker1 :: forall ps. ExplicitMember "worker1" ps => Member "worker1" ps
worker1 = explicitMember
worker2 :: forall ps. ExplicitMember "worker2" ps => Member "worker2" ps
worker2 = explicitMember

quicksort ::
  (KnownSymbol a, KnownSymbol b, KnownSymbol c, Known [Party] ps, Monad m) =>
  Member a ps ->
  Member b ps ->
  Member c ps ->
  Located '[a] [Int] ->
  Choreo ps m (Located '[a] [Int])
quicksort a b c lst = do
  isEmpty <- a `locally` \un -> pure (null (un alone lst))
  broadcast (a, isEmpty) >>= \case
    True -> do
      a `_locally` pure []
    False -> do
      smaller <- (a, \un -> let x : xs = un alone lst in pure [i | i <- xs, i <= x]) ~~> b @@ nobody
      smaller' <- quicksort b c a smaller
      smaller'' <- (b, smaller') ~> a @@ nobody
      bigger <- (a, \un -> let x : xs = un alone lst in pure [i | i <- xs, i > x]) ~~> c @@ nobody
      bigger' <- quicksort c a b bigger
      bigger'' <- (c, bigger') ~> a @@ nobody
      a `locally` \un -> pure $ un alone smaller'' ++ [unsafeQuietHead (un alone lst)] ++ un alone bigger''

sort :: Choreo Participants (CLI IO) ()
sort = do
  lst <- primary `_locally` getInput "The list of numbers to sort:"
  sorted <- quicksort primary worker1 worker2 lst
  primary `locally_` \un -> putOutput "Sorted result:" (un alone sorted)

main :: IO ()
main = easyMain sort
