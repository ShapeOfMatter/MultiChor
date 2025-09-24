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
import EasyMain (easyMain)
import GHC.TypeLits (KnownSymbol)

$(mkLoc "primary")
$(mkLoc "worker1")
$(mkLoc "worker2")

type Participants = ["primary", "worker1", "worker2"]

quicksort ::
  (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbols ps, Monad m) =>
  Member a ps ->
  Member b ps ->
  Member c ps ->
  Located '[a] [Int] ->
  Choreo ps m (Located '[a] [Int])
quicksort a b c lst = do
  isEmpty <- a `locally` \un -> pure (null (un singleton lst))
  broadcast (a, isEmpty) >>= \case
    True -> do
      a `_locally` pure []
    False -> do
      smaller <- (a, \un -> let x : xs = un singleton lst in pure [i | i <- xs, i <= x]) ~~> b @@ nobody
      smaller' <- quicksort b c a smaller
      smaller'' <- (b, smaller') ~> a @@ nobody
      bigger <- (a, \un -> let x : xs = un singleton lst in pure [i | i <- xs, i > x]) ~~> c @@ nobody
      bigger' <- quicksort c a b bigger
      bigger'' <- (c, bigger') ~> a @@ nobody
      a `locally` \un -> pure $ un singleton smaller'' ++ [unsafeQuietHead (un singleton lst)] ++ un singleton bigger''

sort :: Choreo Participants (CLI IO) ()
sort = do
  lst <- primary `_locally` getInput "The list of numbers to sort:"
  sorted <- quicksort primary worker1 worker2 lst
  primary `locally_` \un -> putOutput "Sorted result:" (un singleton sorted)

main :: IO ()
main = easyMain sort
