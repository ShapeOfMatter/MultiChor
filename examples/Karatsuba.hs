{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

{-
# Example: Karatsuba Fast Multiplication

## Overview

This example implements the three-way concurrent implementation of the Karatsuba fast multiplication algorithm shown in [Object-Oriented Choreographic Programming](https://arxiv.org/abs/2005.09520).

## Execution

The executable takes two integers as arguments and returns their product. It uses the local backend and distribute the computation over threads.

```bash
cabal run karatsuba 100 200
20000
```

`Reference.h` contains a single-threaded reference implementation of the algorithm.
-}



module Karatsuba where

import Choreography (runChoreography)
import Choreography.Choreo
import Choreography.Location
import Choreography.Network.Local
import Control.Concurrent.Async (async, mapConcurrently_, wait)
import Data.Proxy
import GHC.TypeLits (KnownSymbol)
import System.Environment

reference :: Integer -> Integer -> Integer
reference n1 n2 =
  if n1 < 10 || n2 < 10
    then n1 * n2
    else result
  where
    log10 :: Integer -> Double
    log10 = logBase 10 . fromIntegral
    m = (max (log10 n1) (log10 n2)) + 1
    m2 = floor (m / 2)
    splitter = 10 ^ m2
    h1 = n1 `div` splitter
    l1 = n1 `mod` splitter
    h2 = n2 `div` splitter
    l2 = n2 `mod` splitter
    z0 = reference l1 l2
    z2 = reference h1 h2
    z1 = (reference (l1 + h1) (l2 + h2)) - z2 - z0
    result = z2 * splitter * splitter + z1 * splitter + z0


primary :: Proxy "primary"
primary = Proxy

worker1 :: Proxy "worker1"
worker1 = Proxy

worker2 :: Proxy "worker2"
worker2 = Proxy

type Participants = ["primary", "worker1", "worker2"]

data KaratsubaNums = KaratsubaNums
  { splitter :: Integer,
    h1 :: Integer,
    h2 :: Integer,
    l1 :: Integer,
    l2 :: Integer
  }

karatsuba ::
  (KnownSymbol a, KnownSymbol b, KnownSymbol c,
   Member a Participants, Member b Participants, Member c Participants) =>
  Proxy a ->
  Proxy b ->
  Proxy c ->
  (Integer @ a) ->
  (Integer @ a) ->
  Choreo Participants IO (Integer @ a)
karatsuba a b c n1 n2 = do
  done <- a `locally` \unwrap -> return $ unwrap n1 < 10 || unwrap n2 < 10
  cond
    (a, done)
    \case
      True -> do
        a `locally` \unwrap -> return $ unwrap n1 * unwrap n2
      False -> do
        x <- a `locally` \unwrap -> return $ f (unwrap n1) (unwrap n2)
        l1' <- (a, \unwrap -> return $ l1 (unwrap x)) ~~> b
        l2' <- (a, \unwrap -> return $ l2 (unwrap x)) ~~> b
        h1' <- (a, \unwrap -> return $ h1 (unwrap x)) ~~> c
        h2' <- (a, \unwrap -> return $ h2 (unwrap x)) ~~> c
        z0' <- karatsuba b c a l1' l2'
        z0 <- (b, z0') ~> a
        z2' <- karatsuba c a b h1' h2'
        z2 <- (c, z2') ~> a
        s1 <- a `locally` \unwrap -> return $ l1 (unwrap x) + h1 (unwrap x)
        s2 <- a `locally` \unwrap -> return $ l2 (unwrap x) + h2 (unwrap x)
        z1' <- karatsuba a b c s1 s2
        z1 <- a `locally` \unwrap -> return $ unwrap z1' - unwrap z2 - unwrap z0
        a `locally` \unwrap -> return let s = splitter (unwrap x) in (unwrap z2 * s * s) + (unwrap z1 * s) + unwrap z0
        where
          f n1 n2 = KaratsubaNums {splitter = splitter, h1 = h1, l1 = l1, h2 = h2, l2 = l2}
            where
              log10 :: Integer -> Double
              log10 = logBase 10 . fromIntegral
              m = max (log10 n1) (log10 n2) + 1
              m2 = floor (m / 2)
              splitter = 10 ^ m2
              h1 = n1 `div` splitter
              l1 = n1 `mod` splitter
              h2 = n2 `div` splitter
              l2 = n2 `mod` splitter

mainChoreo :: Integer -> Integer -> Choreo Participants IO ()
mainChoreo n1 n2 = do
  n1 <- primary `locally` \_ -> return n1
  n2 <- primary `locally` \_ -> return n2
  result <- karatsuba primary worker1 worker2 n1 n2
  primary `locally` \unwrap -> do
    print (unwrap result)
    return ()
  return ()

main :: IO ()
main = do
  [n1, n2] <- map read <$> getArgs
  config <- mkLocalConfig locs
  mapConcurrently_ (runChoreography config (mainChoreo n1 n2)) locs
  return ()
  where
    locs = ["primary", "worker1", "worker2"]
