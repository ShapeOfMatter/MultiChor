{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE TemplateHaskell #-}

module MPCSum where

import Choreography
import Control.Monad
import System.Environment
import Logic.Propositional (introAnd)
import CLI

-- Multiple servers
-- Multiple clients
$(mkLoc "server1")
$(mkLoc "server2")
$(mkLoc "server3")
$(mkLoc "server4")

$(mkLoc "client1")
$(mkLoc "client2")
$(mkLoc "client3")
$(mkLoc "client4")
$(mkLoc "client5")
$(mkLoc "client6")

type Fp = Integer -- field elements

type Servers = ["server1", "server2", "server3", "server4"]
type Clients = ["client1", "client2", "client3", "client4", "client5", "client6"]
-- type Participants = Servers ++ Clients
type Participants = Clients

-- sendShares :: Located Fp c -> Member s Servers -> Choreo Participants (CLI m) (Located Fp s)
-- sendShares share prf = do
--   fps <- (c `introAnd` c, share) ~> s @@ nobody
--   return fps

-- createShares :: Choreo Clients (CLI m) (Faceted Clients [Fp])
-- createShares = do
--   facetedM Clients (do
--                        secret <- getstr
--                        let shares = secretShare secret (length Clients)
--                        facetedM sendShares (zip shares Servers))
--   return $ faceted shares

-- serverSum :: Choreo Participants (CLI m) (Located server1 Fp, Located server2 Fp)
-- serverSum = do
--   s11, s12 <- client1 `_locally` getstr "enter secret" >>= secretShare
--   s21, s22 <- client2 `_locally` getstr "enter secret" >>= secretShare
--   s11s <- (client1 `introAnd` client1, s11) ~> server1 @@ nobody
--   s21s <- (client2 `introAnd` client2, s21) ~> server1 @@ nobody
--   s12s <- (client1 `introAnd` client1, s12) ~> server2 @@ nobody
--   s22s <- (client2 `introAnd` client2, s22) ~> server2 @@ nobody
--   sum1 <- (server1, \un -> return $ (un s11s) + (un s21s)) ~~> server2 @@ nobody
--   sum2 <- (server2, \un -> return $ (un s12s) + (un s22s)) ~~> server1 @@ nobody
--   total1 <- server1 `locally` \un -> return $ (un sum1) + (un sum2)
--   total2 <- server2 `locally` \un -> return $ (un sum1) + (un sum2)
--   return $ (total1, total2)

secretShare :: CLI m (Fp, Fp)
secretShare = do
  secret <- getInput "secret:"
  return (5, secret - 5)

p2pSum :: Choreo Participants (CLI m) ()
p2pSum = do
  shares1 <- client1 `locally` \_ -> secretShare
  shares2 <- client2 `locally` \_ -> secretShare
  s12s <- (client1, \un -> return $ snd $ un client1 shares1) ~~> client2 @@ nobody
  s21s <- (client2, \un -> return $ snd $ un client2 shares2) ~~> client1 @@ nobody
  sum1 <- (client1, \un -> return $ (fst $ un client1 shares1) + (un client1 s21s)) ~~> client1 @@ client2 @@ nobody
  sum2 <- (client2, \un -> return $ (un client2 s12s) + (fst $ un client2 shares2)) ~~> client1 @@ client2 @@ nobody
  total1 <- client1 `locally` \un -> return $ (un client1 sum1) + (un client1 sum2)
  total2 <- client2 `locally` \un -> return $ (un client2 sum1) + (un client2 sum2)
  client1 `locally_` \un -> putOutput "Total:" $ un client1 total1
  client2 `locally_` \un -> putOutput "Total:" $ un client2 total2


main :: IO ()
main = do
  [loc] <- getArgs
  delivery <- case loc of
    "client1"  -> runCLIIO $ runChoreography cfg p2pSum "client1"
    "client2" -> runCLIIO $ runChoreography cfg p2pSum "client2"
    _ -> error "unknown party"
  print delivery
  where
    cfg = mkHttpConfig [ ("client1",  ("localhost", 4242))
                       , ("client2", ("localhost", 4343))
                       ]
