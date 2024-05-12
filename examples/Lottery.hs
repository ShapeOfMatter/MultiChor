{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MonoLocalBinds #-}

module Lottery where

import CLI
import Choreography
import Choreography.Network.Http (API)
import Control.Monad (replicateM)
import Control.Monad.Cont (MonadIO, liftIO)
import Data.Maybe (fromJust)
import GHC.TypeLits (KnownSymbol)
import Logic.Classes (refl)
import System.Environment
import System.Random (randomIO)


-- | Issue #27
-- TODO just a stub for now
-- TODO make this nicer when we have fanin and fanout to avoid redundent code

-- Multiple servers
-- Multiple clients
$(mkLoc "server1")
$(mkLoc "server2")


$(mkLoc "client1")
$(mkLoc "client2")


-- TODO fix later
type Fp = Integer -- field elements


type Participants = ["client1", "client2", "server1", "server2"]


-- Random field in [1 to n]
random :: MonadIO m => CLI m Fp
random = liftIO randomIO


-- | Federated Lottery example from DPrio https://www.semanticscholar.org/paper/DPrio%3A-Efficient-Differential-Privacy-with-High-for-Keeler-Komlo/ae1b2a4e5beaaa850183ad37e0880bb70ae34f4e
lottery
  :: forall clients servers analyst census m _serv1 _serv2 _servTail _client1 _client2 _clientTail
   . ( KnownSymbols clients
     , KnownSymbols servers
     , KnownSymbols '[analyst]
     , (_serv1 ': _serv2 ': _servTail) ~ servers -- There should at least be two servers
     , (_client1 ': _client2 ': _clientTail) ~ clients -- There should at least be two clients
     , MonadIO m
     )
  => Subset clients census -- A proof that clients are part of the census
  -> Subset servers census -- A proof that servers are part of the census
  -> Subset '[analyst] census -- A proof that servers are part of the census
  -- Subset analyst] census -> -- A proof the the analyst is part of the census
  -> Choreo census (CLI m) ()
lottery clients servers analysts = do
  secret <- parallel clients (\_ _ -> getInput "secret:")


  -- A lookup table that maps Server to share to send
  clientShares <-
    clients `parallel` \client un -> do
      freeShares :: [Fp] <- case serverNames of
        [] -> return [] -- This can't actually happen/get used...
        _ : others -> replicateM (length others) $ liftIO randomIO
      let lastShare = un client secret - sum freeShares -- But freeShares could really be empty!
      return $ serverNames `zip` (lastShare : freeShares)

  serverShares <- servers
    `fanOut` ( \server ->
                fanIn
                  clients
                  (inSuper servers server @@ nobody)
                  ( \client ->
                      ( inSuper clients client
                      , \un ->
                            let serverName = toLocTm server
                                share = fromJust $ lookup serverName $ un client clientShares
                             in return share
                      )
                        ~~> inSuper servers server @@ nobody
                  )
             )

  -- Servers each commit to some random value
  randomCommit <- parallel servers (\_ _ -> random)

  -- Servers each send their randomly commits to all other servers
  -- I was thinking we don't need to actually restrict to only other servers besides the current (just don't use randomCommit again only allCommits)
  allCommits <-
    servers
      `fanOut` ( \currServer ->
                  fanIn
                    servers
                    (inSuper servers currServer @@ nobody)
                    ( \recServer ->
                        (inSuper servers currServer, \un -> pure $ un currServer randomCommit) ~~> inSuper servers currServer @@ nobody
                                                                                                                -- \^^ TODO I was expecting recServer but compiler wants curr server
                    )
               )

  -- Sum all shares
  -- TODO modular sum
  -- TODO this is any for some reason. Something is wrong.
  r <- servers `parallel` (\server un -> pure $ sum $ un server allCommits)

  -- Servers each forward share to an analyist s_R^j we end up with a Faceted but only for a single analyst
  -- TODO that's a bit weird? Should be able to get rid of Faceted for a single location
  allShares <- analysts
    `fanOut` ( \analyst ->
                fanIn
                  servers
                  (inSuper analysts analyst @@ nobody)
                  ( \server ->
                      ( inSuper servers server
                      , \un -> pure ((un server $ serverShares) !! (fromIntegral $ un server $  r))
                      )
                        ~~> inSuper analysts analyst @@ nobody
                  )
             )
  -- analyst combines allShares
   -- analysts `parallel` (\analyst un -> un analyst $ sum allShares)

  pure undefined
 where
  serverNames = toLocs servers
  -- I wonder if we can use helpers to make GDP and programs more distinct/clear?
  -- A proof that server is in servers
  proveServerIsInServers server = inSuper servers server @@ nobody

main :: IO ()
main = undefined
