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
import Data.Bifunctor (first)
import qualified Data.Binary as Binary
import Data.ByteString (toStrict)
import Data.FiniteField (primeField)
import Data.Maybe (fromJust)
import GHC.TypeLits (KnownSymbol)
import System.Environment (getArgs)
import System.Random (Random, random, randomR, randomIO, randomRIO)
import Test.QuickCheck (Arbitrary, arbitrary, arbitraryBoundedEnum)

import Choreography
import Choreography.Network.Http
import CLI
import Data (TestArgs, reference)

-- | Arbitrary "large" prime
newtype Fp = Fp $(primeField 999983)                           -- AFAICT the hint warning needs a newer version of hlint to go away:
           deriving (Bounded, Enum, Eq, Ord, Num, Read, Show)  -- https://github.com/ndmitchell/hlint/issues/1226

instance Random Fp where
  random g = toEnum `first` randomR (fromEnum @Fp minBound, fromEnum @Fp maxBound) g
  randomR (l, h) g = toEnum `first` randomR (fromEnum l, fromEnum h) g

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
   . ( KnownSymbols clients, KnownSymbols servers, KnownSymbol analyst, MonadIO m
     , (_serv1 ': _serv2 ': _servTail) ~ servers -- There must be at least be two servers
     , (_client1 ': _client2 ': _clientTail) ~ clients -- There must be at least be two clients
     )
  => Subset clients census -> Subset servers census -> Member analyst census
  -> Choreo census (CLI m) ()
lottery clients servers analyst = do
  secret <- _parallel clients (getInput @Fp "secret:")
  clientShares <- clients `parallel` \client un -> do
      freeShares <- liftIO $ replicateM (length serverNames - 1) $ randomIO @Fp
      return $ serverNames `zip` (un client secret - sum freeShares : freeShares)
  serverShares <- fanOut servers (\server ->
      fanIn clients (inSuper servers server @@ nobody) (\client -> do
          serverShare <- inSuper clients client `locally` \un ->
                           pure $ fromJust $ lookup (toLocTm server) $ un client clientShares
          (inSuper clients client, serverShare) ~> inSuper servers server @@ nobody
        )
    )
  -- 1) Each server selects a random number; τ is some multiple of the number of clients.
  ρ <- _parallel servers (getInput "A random number from 1 to τ:")
  -- Salt value
  ψ <- _parallel servers (randomRIO (2^(18::Int), 2^(20::Int)))
  -- 2) Each server computes and publishes the hash α = H(ρ, ψ) to serve as a commitment
  α <- parallel servers \server un -> pure $ hash (un server ψ) (un server ρ)
  α' <- fanIn servers servers ( \server -> (server, servers, α) ~> servers )
  -- 3) Every server opens their commitments by publishing their ψ and ρ to each other
  ψ' <- fanIn servers servers ( \server -> (server, servers, ψ) ~> servers )
  ρ' <- fanIn servers servers ( \server -> (server, servers, ρ) ~> servers )
  -- 4) All servers verify each other's commitment by checking α = H(ρ, ψ)
  parallel_ servers (\server un ->
      unless (un server α' == zipWith hash (un server ψ') (un server ρ'))
             (liftIO $ throwIO CommitmentCheckFailed)
    )
  -- 5) If all the checks are successful, then sum random values to get the random index.
  ω <- servers `congruently` (\un -> sum (un refl ρ') `mod` length (toLocs clients))
  chosenShares <- servers `parallel` (\server un -> pure $ un server serverShares !! un server ω)
  -- Servers forward shares to an analyist.
  allShares <- fanIn servers (analyst @@ nobody) (\server ->
      (server, servers, chosenShares) ~> analyst @@ nobody
    )
  analyst `locally_` \un -> putOutput "The answer is:" $ sum $ un singleton allShares
 where serverNames = toLocs servers
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
