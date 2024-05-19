{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Lottery where

import CLI
import Choreography
import Control.Monad (replicateM)
import Control.Monad.Cont (MonadIO, liftIO)
import Data.Maybe (fromJust)
import System.Random (randomIO)
import GHC.TypeLits (KnownSymbol)
import Logic.Propositional (introAnd)
import Logic.Classes (refl)


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
-- TODO bound by n
random :: MonadIO m => Fp -> CLI m Fp
random n = liftIO randomIO


-- | Federated Lottery example from DPrio https://www.semanticscholar.org/paper/DPrio%3A-Efficient-Differential-Privacy-with-High-for-Keeler-Komlo/ae1b2a4e5beaaa850183ad37e0880bb70ae34f4e
lottery
  :: forall clients servers analyst census m _serv1 _serv2 _servTail _client1 _client2 _clientTail
   . ( KnownSymbols clients
     , KnownSymbols servers
     , (_serv1 ': _serv2 ': _servTail) ~ servers -- There should at least be two servers
     , (_client1 ': _client2 ': _clientTail) ~ clients -- There should at least be two clients
     , MonadIO m
     , KnownSymbol analyst
     )
  => Subset clients census -- A proof that clients are part of the census
  -> Subset servers census -- A proof that servers are part of the census
  -> Member analyst census -- A proof that servers are part of the census
  -- Subset analyst] census -> -- A proof the the analyst is part of the census
  -> Choreo census (CLI m) ()
lottery clients servers analyst = do
  secret <- parallel clients (\_ _ -> getInput "secret:")

  -- A lookup table that maps Server to share to send
  clientShares <- clients `parallel` \client un -> do
      freeShares :: [Fp] <- case serverNames of
        [] -> return [] -- This can't actually happen/get used...
        _ : others -> replicateM (length others) $ liftIO randomIO
      let lastShare = un client secret - sum freeShares -- But freeShares could really be empty!
      return $ serverNames `zip` (lastShare : freeShares)

  serverShares <- servers `fanOut` ( \server ->
                  fanIn clients (inSuper servers server @@ nobody)
                    ( \client ->
                        ( inSuper clients client
                        , \un ->
                            let serverName = toLocTm server
                                share = fromJust $ lookup serverName $ un client clientShares
                             in return share
                      ) ~~> inSuper servers server @@ nobody
                  )
             )

  -- 1) Each server selects a random number within range [0,τ]
  ρ <- parallel servers (\_ _ -> random τ)

  -- Salt value
  ψ <- parallel servers (\_ _ -> random largeValue)

  -- 2) Each server computes and publishes the hash α = H(ρ, ψ) to serve as a commitment
  α <- fanIn servers servers ( \server ->
                        (inSuper servers server, \un -> pure $ hash (un server ψ) (un server ρ)) ~~> servers
                    )

  -- 3) Every server opens their commitments by publishing their ψ and ρ to each other
  -- Where ₀ represents the opened variants that is Located at all servers rather than Faceted
  ψ₀ <- fanIn servers servers ( \server ->
                        (server `introAnd` inSuper servers server, ψ) ~> servers
                    )

  ρ₀ <- fanIn servers servers ( \server ->
                        (server `introAnd` inSuper servers server, ρ) ~> servers
                    )

  -- 4) All servers verify each other's commitment by checking α = H(ρ, ψ)
  -- TODO hopefully this is in order but if not I should change the types to be [(Loc, a)]
  check <- parallel servers (\server un -> pure $ (un server α) == (uncurry hash <$> zip (un server ψ₀) (un server ρ₀)))


  -- Sum all shares
  -- TODO modular sum
  -- TODO this is any for some reason. Something is wrong.
  r <- servers `replicatively` (\un -> sum $ un refl α)

  -- Servers each forward share to an analyist s_R^j we end up with a Faceted but only for a single analyst
  -- TODO that's a bit weird? Should be able to get rid of Faceted for a single location
  allShares <- fanIn servers (analyst @@ nobody)
                    ( \server ->
                        ( inSuper servers server
                        , \un -> pure (un server serverShares !! fromIntegral (un server r))
                        ) ~~> analyst @@ nobody
                    )

  analyst `locally_` \un -> putOutput "The answer is: " $ sum $ un explicitMember allShares

 where
  serverNames = toLocs servers
  n = length $ toLocs servers
  -- TODO some multiple of n. Is any arbitrary multiple fine or should we do something random
  τ = undefined
  -- TODO maybe not the max? or we can do maxBound / 2
  -- I also kind of mix up ψ and the other ψ looking symbol
  largeValue = undefined
  -- TODO choose some hash function
  hash :: Fp -> Fp -> Fp
  hash ρ ψ = undefined

main :: IO ()
main = undefined
