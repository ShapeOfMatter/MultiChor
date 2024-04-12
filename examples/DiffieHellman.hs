{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}

{-
# Example: diffie-hellman key exchange

## Overview

This example implements the [diffie-hellman key exchange protocol](https://en.wikipedia.org/wiki/Diffie%E2%80%93Hellman_key_exchange).

In this example, two locations, `alice` and `bob`, exchange the secret key without sending the key over the network.

## Execution

To run this example, you will need to run two locations (alice and bob) at the same time. Alice initiates the exchange, and Bob waits for Alice. When both locations are ready, press "enter" on Alice's terminal to start the protocol.

```
> cabal run diffiehellman bob
waiting for alice to initiate key exchange

# on a different terminal
> cabal run diffiehellman alice
enter to start key exchange...
[Enter]

# Alice's terminal
alice's shared key: 1544

# Bob's terminal
bob's shared key: 1544
```

This sample uses [`System.Random`](https://hackage.haskell.org/package/random-1.2.1.1/docs/System-Random.html) and will generate different keys at each invocation.
-}

module DiffieHellman where

import Choreography (mkHttpConfig, runChoreography)
import Choreography.Choreo
import Choreography.Location
import Data.Proxy
import System.Environment
import System.Random

-- helper functions around prime number
-- https://nulldereference.wordpress.com/2012/02/04/generating-prime-numbers-with-haskell/
divisors :: Integer -> [Integer]
divisors 1 = [1]
divisors x = 1 : [y | y <- [2 .. (x `div` 2)], x `mod` y == 0] ++ [x]

isPrime :: Integer -> Bool
isPrime x = divisors x == [1, x]

primeNums :: [Integer]
primeNums = [x | x <- [2 ..], isPrime x]

-- set up proxies
alice :: Proxy "alice"
alice = Proxy

bob :: Proxy "bob"
bob = Proxy

type Participants = ["alice", "bob"]

diffieHellman :: Choreo Participants IO (Integer @ "alice", Integer @ "bob")
diffieHellman = do
  -- wait for alice to initiate the process
  _ <- alice `locally` \_ -> do
    putStrLn "enter to start key exchange..."
    getLine
  bob `locally_` \_ -> do
    putStrLn "waiting for alice to initiate key exchange"

  -- alice picks p and g and sends them to bob
  pa <-
    alice `locally` \_ -> do
      x <- randomRIO (200, 1000 :: Int)
      return $ primeNums !! x
  pb <- (alice, pa) ~> bob
  ga <- alice `locally` \un -> do randomRIO (10, un pa)
  gb <- (alice, ga) ~> bob

  -- alice and bob select secrets
  a <- alice `locally` \_ -> do randomRIO (200, 1000 :: Integer)
  b <- bob `locally` \_ -> do randomRIO (200, 1000 :: Integer)

  -- alice and bob computes numbers that they exchange
  a' <- alice `locally` \un -> do return $ un ga ^ un a `mod` un pa
  b' <- bob `locally` \un -> do return $ un gb ^ un b `mod` un pb

  -- exchange numbers
  a'' <- (alice, a') ~> bob
  b'' <- (bob, b') ~> alice

  -- compute shared key
  s1 <-
    alice `locally` \un ->
      let s = un b'' ^ un a `mod` un pa
       in do
            putStrLn ("alice's shared key: " ++ show s)
            return s
  s2 <-
    bob `locally` \un ->
      let s = un a'' ^ un b `mod` un pb
       in do
            putStrLn ("bob's shared key: " ++ show s)
            return s
  return (s1, s2)

main :: IO ()
main = do
  [loc] <- getArgs
  _ <- case loc of
    "alice" -> runChoreography config diffieHellman "alice"
    "bob" -> runChoreography config diffieHellman "bob"
    _ -> error "unknown party"
  return ()
  where
    config =
      mkHttpConfig
        [ ("alice", ("localhost", 5000)),
          ("bob", ("localhost", 5001))
        ]
