{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Lottery where

import CLI
import Choreography
import Choreography.Network.Http
import Control.Exception (Exception, throwIO)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Crypto.Hash (Digest)
import Crypto.Hash qualified as Crypto
import Data (TestArgs, reference)
import Data.Bifunctor (first)
import Data.Binary qualified as Binary
import Data.ByteString (toStrict)
import Data.FiniteField (primeField)
import Data.Foldable (toList)
import GHC.TypeLits (KnownSymbol)
import System.Environment (getArgs)
import System.Random (Random, random, randomIO, randomR, randomRIO)
import Test.QuickCheck (Arbitrary, arbitrary, arbitraryBoundedEnum)

-- | Arbitrary "large" prime
newtype Fp = Fp $(primeField 999983) -- AFAICT the hint warning needs a newer version of hlint to go away:
  deriving (Bounded, Enum, Eq, Ord, Num) -- https://github.com/ndmitchell/hlint/issues/1226

instance Read Fp where
  readsPrec _ s = [(Fp (read s), "")]

instance Show Fp where
  show (Fp x) = show x

instance Random Fp where
  random g = toEnum `first` randomR (fromEnum @Fp minBound, fromEnum @Fp maxBound) g
  randomR (l, h) g = toEnum `first` randomR (fromEnum l, fromEnum h) g

instance Arbitrary Fp where
  arbitrary = arbitraryBoundedEnum

data LotteryError = CommitmentCheckFailed deriving (Show)

instance Exception LotteryError

data Args = Args
  { secrets :: (Fp, Fp, Fp, Fp, Fp), -- we lock our test to the case of five clients and three servers
    randomIs :: (Int, Int, Int)
  }
  deriving (Eq, Show, Read)

instance TestArgs Args Fp where
  reference Args {secrets = (c1, c2, c3, c4, c5), randomIs = (s1, s2, s3)} =
    let i = (s1 + s2 + s3) `mod` 5
     in [c1, c2, c3, c4, c5] !! i

instance Arbitrary Args where
  arbitrary =
    Args
      <$> ((,,,,) <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary)
      <*> ((,,) <$> arbitrary <*> arbitrary <*> arbitrary)

-- | Federated Lottery example from DPrio https://www.semanticscholar.org/paper/DPrio%3A-Efficient-Differential-Privacy-with-High-for-Keeler-Komlo/ae1b2a4e5beaaa850183ad37e0880bb70ae34f4e
lottery ::
  forall clients servers analyst census m _serv1 _serv2 _servTail _client1 _client2 _clientTail.
  ( KnownSymbols clients,
    KnownSymbols servers,
    KnownSymbol analyst,
    MonadIO m,
    (_serv1 ': _serv2 ': _servTail) ~ servers, -- There must be at least be two servers
    (_client1 ': _client2 ': _clientTail) ~ clients -- There must be at least be two clients
  ) =>
  Subset clients census ->
  Subset servers census ->
  Member analyst census ->
  Choreo census (CLI m) ()
lottery clients servers analyst = do
  secret <- _parallel clients (getInput @Fp "secret:")
  clientShares <-
    clients `parallel` \client un ->
      ( case tySpine @servers of
          TyCons -> do
            -- I guess this explains to GHC that we have KnownSymbols _servTail? IDK
            freeShares <- liftIO $ sequence $ pure $ randomIO @Fp
            return $ (viewFacet un client secret - sum freeShares) `qCons` freeShares
      )
  serverShares <-
    fanOut
      ( \server ->
          -- Probably I've already got a nicer way to write this on hand; idk.
          fanIn
            (inSuper servers server @@ nobody)
            ( \client -> do
                serverShare <-
                  inSuper clients client `locally` \un ->
                    pure $ viewFacet un client clientShares `getLeaf` server
                (inSuper clients client, serverShare) ~> inSuper servers server @@ nobody
            )
      )
  -- 1) Each server selects a random number; τ is some multiple of the number of clients.
  ρ <- _parallel servers (getInput "A random number from 1 to τ:")
  -- Salt value
  ψ <- _parallel servers (randomRIO (2 ^ (18 :: Int), 2 ^ (20 :: Int)))
  -- 2) Each server computes and publishes the hash α = H(ρ, ψ) to serve as a commitment
  α <- parallel servers \server un -> pure $ hash (viewFacet un server ψ) (viewFacet un server ρ)
  α' <- gather servers servers α
  -- 3) Every server opens their commitments by publishing their ψ and ρ to each other
  ψ' <- gather servers servers ψ
  ρ' <- gather servers servers ρ
  -- 4) All servers verify each other's commitment by checking α = H(ρ, ψ)
  parallel_
    servers
    ( \server un ->
        unless
          (un server α' == (hash <$> un server ψ' <*> un server ρ'))
          (liftIO $ throwIO CommitmentCheckFailed)
    )
  -- 5) If all the checks are successful, then sum random values to get the random index.
  ω <- servers `congruently` (\un -> sum (un refl ρ') `mod` length (toLocs clients))
  chosenShares <- servers `parallel` (\server un -> pure $ toList (viewFacet un server serverShares) !! un server ω)
  -- Servers forward shares to an analyist.
  allShares <- gather servers (analyst @@ nobody) chosenShares
  analyst `locally_` \un -> putOutput "The answer is:" $ sum $ un singleton allShares
  where
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
