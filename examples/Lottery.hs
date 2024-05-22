{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Lottery where

import CLI
import Choreography
import Control.Monad ( replicateM, unless )
import Control.Monad.Cont (MonadIO, liftIO)
import Data.Maybe (fromJust)
import System.Random (randomIO, Random)
import GHC.TypeLits (KnownSymbol)
import Logic.Propositional (introAnd)
import Logic.Classes (refl)
import Control.Exception (throwIO)
import GHC.Exception (Exception)
import qualified Crypto.Hash as Crypto
import qualified Data.Binary as Binary
import Crypto.Hash (Digest)
import Data.ByteString (ByteString, toStrict)
import qualified GHC.TypeLits as TL
import Data.Data (Proxy(..))
import System.Environment (getArgs)

-- Multiple servers
-- Multiple clients
$(mkLoc "server1")
$(mkLoc "server2")


$(mkLoc "client1")
$(mkLoc "client2")


newtype Fp (prime :: TL.Nat) = Fp Integer deriving (Show, Eq)

toFp :: forall prime. (TL.KnownNat prime) => Integer -> Fp prime
toFp i = Fp $ mod i (TL.natVal $ Proxy @prime)

unsafeToInt :: forall prime. (TL.KnownNat prime) => Fp prime -> Integer
unsafeToInt (Fp i) = i

instance (TL.KnownNat prime) => Num (Fp prime) where
  (+) (Fp x) (Fp y) = toFp $ x + y
  (*) (Fp x) (Fp y) = toFp $ x * y
  -- TODO rest of implementation
  -- Just realized there's an existing finite field library. Not sure if we'd prefer this or the other

instance Read (Fp prime) -- TODO
instance Random (Fp prime) -- TODO


type Participants = ["client1", "client2", "server1", "server2"]


-- Random field in [1 to n]
-- TODO bound by n
random :: MonadIO m => Fp p -> CLI m (Fp p)
random n = liftIO randomIO

data LotteryError = CommitmentCheckFailed deriving (Show)

instance Exception LotteryError


-- | Federated Lottery example from DPrio https://www.semanticscholar.org/paper/DPrio%3A-Efficient-Differential-Privacy-with-High-for-Keeler-Komlo/ae1b2a4e5beaaa850183ad37e0880bb70ae34f4e
lottery
  :: forall clients servers analyst census m _serv1 _serv2 _servTail _client1 _client2 _clientTail p
   . ( KnownSymbols clients
     , KnownSymbols servers
     , (_serv1 ': _serv2 ': _servTail) ~ servers -- There should at least be two servers
     , (_client1 ': _client2 ': _clientTail) ~ clients -- There should at least be two clients
     , MonadIO m
     , KnownSymbol analyst
     , TL.KnownNat p -- Finite Field
     )
  => Subset clients census -- A proof that clients are part of the census
  -> Subset servers census -- A proof that servers are part of the census
  -> Member analyst census -- A proof that the analyst is part of the census
  -- Subset analyst] census -> -- A proof the the analyst is part of the census
  -> Choreo census (CLI m) ()
lottery clients servers analyst = do
  secret <- parallel clients (\_ _ -> getInput @(Fp p) "secret:")

  -- A lookup table that maps Server to share to send
  clientShares <- clients `parallel` \client un -> do
      freeShares :: [Fp p] <- case serverNames of
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
  α <- fanIn servers servers ( \server -> (inSuper servers server, \un -> pure $ hash (un server ψ) (un server ρ)) ~~> servers)

  -- 3) Every server opens their commitments by publishing their ψ and ρ to each other
  -- Where ₀ represents the opened variants that is Located at all servers rather than Faceted
  ψ₀ <- fanIn servers servers ( \server -> (server `introAnd` inSuper servers server, ψ) ~> servers)

  ρ₀ <- fanIn servers servers ( \server -> (server `introAnd` inSuper servers server, ρ) ~> servers)

  -- 4) All servers verify each other's commitment by checking α = H(ρ, ψ)
  -- TODO hopefully this is in order but if not I should change the types to be [(Loc, a)]
  _ <- parallel servers (\server un -> do
                                unless (un server α == (uncurry hash <$> zip (un server ψ₀) (un server ρ₀)))
                                  (liftIO $ throwIO CommitmentCheckFailed)
                            )

  -- 5) If all the checks are successfull. Then sum shares.
  -- TODO need to get a Located Bool servers from the Faceted Bool servers or something then a Cond
  -- Where ω is an index on the shares
  -- TODO modular sum
  ω <- servers `replicatively` (\un -> sum $ un refl ρ₀)

  -- Servers each forward share to an analyist s_R^j we end up with a Located but only for a single analyst
  allShares <- fanIn servers (analyst @@ nobody)
                    ( \server ->
                        ( inSuper servers server
                        , \un -> pure (un server serverShares !! fromIntegral (unsafeToInt (un server ω)))
                        ) ~~> analyst @@ nobody
                    )

  analyst `locally_` \un -> putOutput "The answer is: " $ sum $ un explicitMember allShares

 where
  serverNames = toLocs servers
  n = length $ toLocs servers
  -- This is some multiple of n. I'm just choosing n to make it simpler for now.
  τ :: Fp p
  τ = fromIntegral n
  largeValue :: Fp p
  largeValue = Fp $ TL.natVal (Proxy @p) - 1
  -- TODO now sure how I properly do  ρ || ψ
  -- I just encoded to bytes then concated them
  hash :: Fp p -> Fp p -> Digest Crypto.SHA256
  hash (Fp ρ) (Fp ψ) = Crypto.hash $ toStrict (Binary.encode ρ <> Binary.encode ψ)

main :: IO ()
main = do
  [loc] <- getArgs
  let clientProof = undefined
      serverProof = undefined
      analystProof = undefined
  _ <- case loc of
    "client1" -> runCLIIO $ runChoreography config (lottery clientProof serverProof analystProof) "client1"
    "client2" -> runCLIIO $ runChoreography config (lottery clientProof serverProof analystProof) "client2"
    "server1" -> runCLIIO $ runChoreography config (lottery clientProof serverProof analystProof) "server1"
    "server2" -> runCLIIO $ runChoreography config (lottery clientProof serverProof analystProof) "server2"
    "analyst" -> runCLIIO $ runChoreography config (lottery clientProof serverProof analystProof) "analyst"
    _ -> error "unknown party"
  return ()
  where
    config =
      mkHttpConfig
        [ ("client1", ("localhost", 5000)),
          ("client2", ("localhost", 5001)),
          ("server1", ("localhost", 5002)),
          ("server2", ("localhost", 5003)),
          ("analyst", ("localhost", 5004))
        ]
