{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module ObliviousTransfer where

import Choreography
--import Control.Monad
import Control.Monad.Cont (MonadIO)
import System.Environment
--import Logic.Propositional (introAnd)
import CLI
import System.Random
import GHC.TypeLits (KnownSymbol)

import qualified Sel.PublicKey.Cipher as Cipher

-- Multiple servers
-- Multiple clients
$(mkLoc "client1")
$(mkLoc "client2")

-- p2pSum :: (MonadIO m) => Choreo Participants (CLI m) ()
-- p2pSum = do
--   shares1 <- client1 `locally` \_ -> secretShare
--   shares2 <- client2 `locally` \_ -> secretShare
--   s12s <- (client1, \un -> return $ snd $ un client1 shares1) ~~> client2 @@ nobody
--   s21s <- (client2, \un -> return $ snd $ un client2 shares2) ~~> client1 @@ nobody
--   sum1 <- (client1, \un -> return $ (fst $ un client1 shares1) + (un client1 s21s)) ~~> client1 @@ client2 @@ nobody
--   sum2 <- (client2, \un -> return $ (un client2 s12s) + (fst $ un client2 shares2)) ~~> client1 @@ client2 @@ nobody
--   total1 <- client1 `locally` \un -> return $ (un client1 sum1) + (un client1 sum2)
--   total2 <- client2 `locally` \un -> return $ (un client2 sum1) + (un client2 sum2)
--   client1 `locally_` \un -> putOutput "Total:" $ un client1 total1
--   client2 `locally_` \un -> putOutput "Total:" $ un client2 total2


--ot :: (KnownSymbol p1, KnownSymbol p2, MonadIO m) => Choreo '[p1 p2] (CLI m) ()
ot :: (KnownSymbol p1, KnownSymbol p2, KnownSymbols ps, MonadIO m) => Member p1 ps -> Member p2 ps -> Choreo ps (CLI m) ()
ot p1 p2 = do
  v1 <- (p1, \un -> return 5) ~~> p2 @@ nobody
  return ()

main :: IO ()
main = do
  [loc] <- getArgs
  delivery <- case loc of
    "client1"  -> runCLIIO $ runChoreography cfg ot "client1"
    "client2" -> runCLIIO $ runChoreography cfg ot "client2"
    _ -> error "unknown party"
  print delivery
  where
    cfg = mkHttpConfig [ ("client1", ("localhost", 4242))
                       , ("client2", ("localhost", 4343))
                       ]
