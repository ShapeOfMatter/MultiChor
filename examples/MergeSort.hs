{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

{-
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

import Choreography (runChoreography)
import Choreography.Choreo
import Choreography.Location
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

sort :: (KnownSymbol a
        ,KnownSymbol c
        ,KnownSymbol b
        ) =>
  Member a ps ->
  Member b ps ->
  Member c ps ->
  ([Int] @ a) ->
  Choreo ps IO ([Int] @ a)
sort a b c lst = do
  condition <- a `locally` \un -> do return $ length (un lst) > 1
  cond (a, condition) \case
    True -> do
      _ <- a `locally` \un -> do return $ length (un lst) `div` 2
      divided <- a `locally` \un -> do return $ divide (un lst)
      l <- a `locally` \un -> do return $ fst (un divided)
      r <- a `locally` \un -> do return $ snd (un divided)
      l' <- (a, l) ~> b
      r' <- (a, r) ~> c
      ls' <- sort b c a l'
      rs' <- sort c a b r'
      merge a b c ls' rs'
    False -> do
      return lst

merge :: (KnownSymbol a
         ,KnownSymbol c
         ,KnownSymbol b
         ) =>
  Member a ps ->
  Member b ps ->
  Member c ps ->
  [Int] @ b ->
  [Int] @ c ->
  Choreo ps IO ([Int] @ a)
merge a b c lhs rhs = do
  lhsHasElements <- b `locally` \un -> do return $ not (null (un lhs))
  cond (b, lhsHasElements) \case
    True -> do
      rhsHasElements <- c `locally` \un -> do return $ not (null (un rhs))
      cond (c, rhsHasElements) \case
        True -> do
          rhsHeadAtC <- c `locally` \un -> do return $ head (un rhs)
          rhsHeadAtB <- (c, rhsHeadAtC) ~> b
          takeLhs <- b `locally` \un -> do return $ head (un lhs) <= un rhsHeadAtB
          cond (b, takeLhs) \case
            True -> do
              -- take (head lhs) and merge the rest
              lhs' <- b `locally` \un -> do return $ tail (un lhs)
              merged <- merge a b c lhs' rhs
              lhsHeadAtB <- b `locally` \un -> do return $ head (un lhs)
              lhsHeadAtA <- (b, lhsHeadAtB) ~> a
              a `locally` \un -> do return $ un lhsHeadAtA : un merged
            False -> do
              -- take (head rhs) and merge the rest
              rhs' <- c `locally` \un -> do return $ tail (un rhs)
              merged <- merge a b c lhs rhs'
              rhsHeadAtC' <- c `locally` \un -> do return $ head (un rhs)
              rhsHeadAtA <- (c, rhsHeadAtC') ~> a
              a `locally` \un -> do return $ un rhsHeadAtA : un merged
        False -> do
          (b, lhs) ~> a
    False -> do
      (c, rhs) ~> a

mainChoreo :: Choreo Participants IO ()
mainChoreo = do
  lst <- primary `locally` \_ -> do return [1, 6, 5, 3, 4, 2, 7, 8]
  sorted <- sort primary worker1 worker2 lst
  _ <- primary `locally` \un -> do
    print (un sorted)
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
