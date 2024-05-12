{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE TemplateHaskell #-}

module Lottery where

import Choreography
import System.Environment
import CLI
import Logic.Classes (refl)
import Choreography.Network.Http (API)
import Control.Monad (replicateM)
import Control.Monad.Cont (liftIO, MonadIO)
import System.Random (randomIO)
import Data.Maybe (fromJust)

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

lottery2 :: forall clients servers census m _h1 _h2 _hs.
  (KnownSymbols clients, KnownSymbols servers, (_h1 ': _h2 ': _hs) ~ servers
  , MonadIO m
  ) =>
  Subset clients census -> -- A proof that clients are part of the census
  Subset servers census -> -- A proof that servers are part of the census
  Choreo census (CLI m) ()
lottery2 clients servers = do
  secret <- parallel clients (\_ _ -> getInput "secret:")

  -- A lookup table that maps Server to share to send
  shares <- clients `parallel` \mem un -> do
    freeShares :: [Fp] <- case serverNames of
                              [] -> return [] -- This can't actually happen/get used...
                              _:others -> replicateM (length others) $ liftIO randomIO
    let lastShare = un mem secret - sum freeShares  -- But freeShares could really be empty!
    return $ serverNames `zip` (lastShare : freeShares)

       -- -> (forall q. (KnownSymbol q) => Member q qs -> Choreo ps m (Located rs a))  -- ^ The body.
       -- -> Choreo ps m (Located rs [a])
  servers `fanOut` (\server ->
                                 fanIn clients (inSuper servers server @@ nobody)
                                  (\client -> (inSuper clients client, (\un ->
                                                                                           let serverName = toLocTm server
                                                                                               share = fromJust $ lookup serverName $ un client shares in
                                                                                           return share
                                                                                        )) ~~> inSuper servers server @@ nobody)
                                 )


  pure undefined
  where
    serverNames = toLocs servers

main :: IO ()
main = undefined
