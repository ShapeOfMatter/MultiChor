
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE TemplateHaskell #-}

module Lottery where

import Choreography
import System.Environment
import CLI
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

-- TODO
secretShare :: CLI m (Fp, Fp)
secretShare = do
  secret <- getInput "secret:"
  return (5, secret - 5)

-- Random field in [1 to n]
random :: CLI m (Fp)
random = undefined


-- TODO arbitrary number of clients and participants >= 2 would be nice
-- I'll move this later just like having the above for reference
-- TODO extend to multiple servers later
lottery :: forall clients m. (KnownSymbols clients) => Choreo ("server1" ': "server2" ': clients) (CLI m) ()
lottery = do
  -- A proof that a subset clients is a subset of clients with servers?
  let clients = refl
  s <- clients `parallel` \_ _ -> secretShare

  -- Create shares
  -- client1Shares <- client1 `locally` (\_ -> secretShare)
  -- client2Shares <- client2 `locally` (\_ -> secretShare)

  -- Client 1 sends shares to all servers {1,2}
  -- client1Share1AtServer1 <- (s, \un -> return $ fst $ un client1 client1Shares) ~~> server1 @@ nobody
  -- client1Share2AtServer2 <- (client1, \un -> return $ snd $ un client1 client1Shares) ~~> server2 @@ nobody

  -- Client 2 sends shares to all servers {1,2}
  -- client2Share1AtServer1 <- (client2, \un -> return $ fst $ un client2 client2Shares) ~~> server1 @@ nobody
  -- client2Share1AtServer2 <- (client2, \un -> return $ snd $ un client2 client2Shares) ~~> server2 @@ nobody

  -- Server each commit to a random value r^j in [1-n]
  -- rj1 <- server1 `locally` \_ -> random
  -- rj2 <- server2 `locally` \_ -> random

  -- -- TODO I think there's a step missing because I don't see what we do with the client secrets

  -- -- The servers each reveal their randomness to all the other servers. R = sum_j(r^j) mod n
  -- rj1AtServer2 <- (server1, \un -> return $ un server1 rj1) ~~> server2 @@ nobody
  -- rj2AtServer1 <- (server2, \un -> return $ un server2 rj2) ~~> server1 @@ nobody


  pure undefined

main :: IO ()
main = undefined
