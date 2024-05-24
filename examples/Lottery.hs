{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Lottery where

import Control.Exception (Exception, throwIO)
import Control.Monad ( replicateM, unless )
import Crypto.Hash (Digest)
import qualified Crypto.Hash as Crypto
import Control.Monad.Cont (MonadIO, liftIO)
import qualified Data.Binary as Binary
import Data.ByteString (toStrict)
import Data.FiniteField (primeField)
import Data.Maybe (fromJust)
import GHC.TypeLits (KnownSymbol)
import Logic.Classes (refl)
import Logic.Propositional (introAnd)
import System.Environment (getArgs)
import System.Random (randomRIO)
import Test.QuickCheck (Arbitrary, arbitrary, arbitraryBoundedEnum)

import Choreography
import Choreography.Network.Http
import CLI
import Data (TestArgs, reference)

-- | Arbitrary "large" prime
newtype Fp = Fp $(primeField 999983)                           -- AFAICT the hint warning needs a newer version of hlint to go away:
           deriving (Bounded, Enum, Eq, Ord, Num, Read, Show)  -- https://github.com/ndmitchell/hlint/issues/1226

instance Arbitrary Fp where
  arbitrary = arbitraryBoundedEnum

data LotteryError = CommitmentCheckFailed deriving (Show)

instance Exception LotteryError


data Args = Args{ secrets :: (Fp, Fp, Fp, Fp, Fp) -- we lock our test to the case of five clients and three servers
                , randomIs :: (Int, Int, Int)
                } deriving (Eq, Show, Read)
instance TestArgs Args Fp where
  reference Args{secrets=(c1, c2, c3, c4, c5), randomIs=(s1, s2, s3)} =
    let i = (s1 + s2 + s3) `mod` 5

    in [c1, c2, c3, c4, c5] !! i

instance Arbitrary Args where
  arbitrary = Args <$> ((,,,,) <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary)
                   <*> ((,,) <$> arbitrary <*> arbitrary <*> arbitrary)




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
  -> Member analyst census -- A proof that the analyst is part of the census
  -- Subset '[analyst] census -> -- A proof the the analyst is part of the census
  -> Choreo census (CLI m) ()
lottery clients servers analyst = do
  secret <- parallel clients (\_ _ -> getInput @Fp "secret:")

  -- A lookup table that maps Server to share to send
  clientShares <- clients `parallel` \client un -> do
      freeShares :: [Fp] <- case serverNames of
        [] -> return [] -- This can't actually happen/get used...
        _ : others -> liftIO $ replicateM (length others) $ toEnum <$> randomRIO (fromEnum @Fp minBound, fromEnum @Fp maxBound)
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
  ρ <- parallel servers (\_ _ -> getInput $ "A random number from 0 to " ++ show τ ++ ":")

  -- Salt value
  ψ <- parallel servers (\_ _ -> randomRIO (largeishValue, largeValue))

  -- 2) Each server computes and publishes the hash α = H(ρ, ψ) to serve as a commitment
  α <- fanIn servers servers ( \server -> (inSuper servers server, \un -> pure $ hash (un server ψ) (un server ρ)) ~~> servers)

  -- 3) Every server opens their commitments by publishing their ψ and ρ to each other
  -- Where ₀ represents the opened variants that is Located at all servers rather than Faceted
  ψ₀ <- fanIn servers servers ( \server -> (server `introAnd` inSuper servers server, ψ) ~> servers)

  ρ₀ <- fanIn servers servers ( \server -> (server `introAnd` inSuper servers server, ρ) ~> servers)

  -- 4) All servers verify each other's commitment by checking α = H(ρ, ψ)
  _ <- parallel servers (\server un -> do
                                unless (un server α == (uncurry hash <$> zip (un server ψ₀) (un server ρ₀)))
                                  (liftIO $ throwIO CommitmentCheckFailed)
                            )

  -- 5) If all the checks are successfull. Then sum shares.
  -- Where ω is an index on the shares
  ω <- servers `congruently` (\un -> sum (un refl ρ₀) `mod` n)

  -- Servers each forward share to an analyist s_R^j we end up with a Located but only for a single analyst
  allShares <- fanIn servers (analyst @@ nobody)
                    ( \server ->
                        ( inSuper servers server
                        , \un -> pure (un server serverShares !! un server ω)
                        ) ~~> analyst @@ nobody
                    )

  analyst `locally_` \un -> putOutput "The answer is:" $ sum $ un explicitMember allShares

 where
  serverNames = toLocs servers
  n = length $ toLocs clients
  τ :: Int
  τ = 3 * n  -- This is "some multiple" of n.
  largeValue :: Int
  largeValue = 2 ^ (20 :: Int)
  largeishValue = 2 ^ (18 :: Int)
  hash :: Int -> Int -> Digest Crypto.SHA256
  hash ρ ψ = Crypto.hash $ toStrict (Binary.encode ρ <> Binary.encode ψ)

main :: IO ()
main = do
  [loc] <- getArgs
  let clientProof :: Subset '["client1", "client2"] '["client1", "client2", "server1", "server2", "analyst"]
      clientProof = explicitSubset
      serverProof :: Subset '["server1", "server2"] '["client1", "client2", "server1", "server2", "analyst"]
      serverProof = explicitSubset
      analystProof :: Member "analyst" '["client1", "client2", "server1", "server2", "analyst"]
      analystProof = explicitMember
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
