{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

{-
# This example was carried over from earlier work, likely HasChor. It doesn't yet have a unit test attached to it.

# Example: merge sort

## Overview

This example implements the three-way concurrent implementation of merge sort shown in [Object-Oriented Choreographic Programming](https://arxiv.org/abs/2005.09520).

## Execution

This choreography will sort the list `[1, 6, 5, 3, 4, 2, 7, 8]`. It requires three locations: `primary`, `worker1`, and `worker2`.

```bash
# start two workers on separate terminals
cabal run mergesort worker1
cabal run mergesort worker2
# start primary on another terminal
cabal run mergesort primary
[1,2,3,4,5,6,7,8]
```
-}

module MergeSort where

import Choreography
import Choreography.Network.Http
import GHC.TypeLits (KnownSymbol)
import System.Environment

divide :: [a] -> ([a], [a])
divide xs = splitAt lhx xs
  where
    lhx = length xs `div` 2

$(mkLoc "primary")
$(mkLoc "worker1")
$(mkLoc "worker2")

type Participants = ["primary", "worker1", "worker2"]

sort ::
  ( KnownSymbol a,
    KnownSymbol c,
    KnownSymbol b,
    KnownSymbols ps
  ) =>
  Member a ps ->
  Member b ps ->
  Member c ps ->
  Located '[a] [Int] ->
  Choreo ps IO (Located '[a] [Int])
sort a b c lst = do
  condition <- a `locally` \un -> do return $ length (un singleton lst) > 1
  broadcast (a, condition) >>= \case
    True -> do
      _ <- a `locally` \un -> do return $ length (un singleton lst) `div` 2
      divided <- a `locally` \un -> do return $ divide (un singleton lst)
      l <- a `locally` \un -> do return $ fst (un singleton divided)
      r <- a `locally` \un -> do return $ snd (un singleton divided)
      l' <- (a, l) ~> b @@ nobody
      r' <- (a, r) ~> c @@ nobody
      ls' <- sort b c a l'
      rs' <- sort c a b r'
      merge a b c ls' rs'
    False -> do
      return lst

merge ::
  ( KnownSymbol a,
    KnownSymbol c,
    KnownSymbol b,
    KnownSymbols ps
  ) =>
  Member a ps ->
  Member b ps ->
  Member c ps ->
  Located '[b] [Int] ->
  Located '[c] [Int] ->
  Choreo ps IO (Located '[a] [Int])
merge a b c lhs rhs = do
  lhsHasElements <- b `locally` \un -> do return $ not (null (un singleton lhs))
  broadcast (b, lhsHasElements) >>= \case
    True -> do
      rhsHasElements <- c `locally` \un -> do return $ not (null (un singleton rhs))
      broadcast (c, rhsHasElements) >>= \case
        True -> do
          rhsHeadAtC <- c `locally` \un -> do return $ head (un singleton rhs)
          rhsHeadAtB <- (c, rhsHeadAtC) ~> b @@ nobody
          takeLhs <- b `locally` \un -> do return $ head (un singleton lhs) <= un singleton rhsHeadAtB
          broadcast (b, takeLhs) >>= \case
            True -> do
              -- take (head lhs) and merge the rest
              lhs' <- b `locally` \un -> do return $ tail (un singleton lhs)
              merged <- merge a b c lhs' rhs
              lhsHeadAtB <- b `locally` \un -> do return $ head (un singleton lhs)
              lhsHeadAtA <- (b, lhsHeadAtB) ~> a @@ nobody
              a `locally` \un -> do return $ un singleton lhsHeadAtA : un singleton merged
            False -> do
              -- take (head rhs) and merge the rest
              rhs' <- c `locally` \un -> do return $ tail (un singleton rhs)
              merged <- merge a b c lhs rhs'
              rhsHeadAtC' <- c `locally` \un -> do return $ head (un singleton rhs)
              rhsHeadAtA <- (c, rhsHeadAtC') ~> a @@ nobody
              a `locally` \un -> do return $ un singleton rhsHeadAtA : un singleton merged
        False -> do
          (b, lhs) ~> a @@ nobody
    False -> do
      (c, rhs) ~> a @@ nobody

mainChoreo :: Choreo Participants IO ()
mainChoreo = do
  lst <- primary `_locally` return [1, 6, 5, 3, 4, 2, 7, 8]
  sorted <- sort primary worker1 worker2 lst
  _ <-
    primary `locally` \un -> do
      print (un primary sorted)
      return ()
  return ()

main :: IO ()
main = do
  [loc] <- getArgs
  case loc of
    "primary" -> runChoreography config mainChoreo "primary"
    "worker1" -> runChoreography config mainChoreo "worker1"
    "worker2" -> runChoreography config mainChoreo "worker2"
    _ -> error "unknown worker"
  return ()
  where
    config =
      mkHttpConfig
        [ ("primary", ("localhost", 3000)),
          ("worker1", ("localhost", 4000)),
          ("worker2", ("localhost", 5000))
        ]
