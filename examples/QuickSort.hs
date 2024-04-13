{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

{-
# Example: Quicksort

## Overview

This example implements the three-way concurrent implementation of quicksort.

## Execution

This choreography will sort the list `[1, 6, 5, 3, 4, 2, 7, 8]`. It uses the local backend and distribute the computation over threads.

```bash
cabal run quicksort
[1,2,3,4,5,6,7,8]
```

`Reference.hs` contains a single-threaded reference implementation of the algorithm.
-}

module QuickSort where

import Control.Concurrent.Async (mapConcurrently_)

import Choreography (runChoreography)
import Choreography.Choreo
import Choreography.Location
import Choreography.Network.Local
import GHC.TypeLits (KnownSymbol)

reference :: [Int] -> [Int]
reference [] = []
reference (x : xs) = smaller ++ [x] ++ bigger
  where
    smaller = reference [a | a <- xs, a <= x]
    bigger = reference [a | a <- xs, a > x]



$(mkLoc "primary")
$(mkLoc "worker1")
$(mkLoc "worker2")
type Participants = ["primary", "worker1", "worker2"]

quicksort :: (KnownSymbol a, KnownSymbol b, KnownSymbol c) =>
             Member a ps -> Member b ps -> Member c ps
             -> [Int] @ a -> Choreo ps IO ([Int] @ a)
quicksort a b c lst = do
  isEmpty <- a `locally` \un -> pure (null (un lst))
  cond (a, isEmpty) \case
    True -> do
      a `locally` \_ -> pure []
    False -> do
      smaller <- (a, \un -> let x : xs = un lst in pure [i | i <- xs, i <= x]) ~~> b
      smaller' <- quicksort b c a smaller
      smaller'' <- (b, smaller') ~> a
      bigger <- (a, \un -> let x : xs = un lst in pure [i | i <- xs, i > x]) ~~> c
      bigger' <- quicksort c a b bigger
      bigger'' <- (c, bigger') ~> a
      a `locally` \un -> pure $ un smaller'' ++ [head (un lst)] ++ un bigger''

mainChoreo :: Choreo Participants IO ()
mainChoreo = do
  lst <- primary `locally` \_ -> do return [1, 6, 5, 3, 4, 2, 7, 8]
  sorted <- quicksort primary worker1 worker2 lst
  primary `locally_` \un -> do
    print (un sorted)
    return ()
  return ()

main :: IO ()
main = do
  config <- mkLocalConfig locations
  mapConcurrently_ (runChoreography config mainChoreo) locations
  return ()
  where
    locations = ["primary", "worker1", "worker2"]
