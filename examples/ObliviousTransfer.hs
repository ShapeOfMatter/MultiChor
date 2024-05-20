{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module ObliviousTransfer where

import Choreography
--import Control.Monad
import Control.Monad.Cont (MonadIO, liftIO)
import System.Environment
--import Logic.Propositional (introAnd)
import CLI
import GHC.TypeLits (KnownSymbol)
import Logic.Propositional (introAnd)
import Logic.Classes (refl) -- Reflexive, Transitive, transitive
import qualified Data.ByteString as BS

import Data.ByteString.Char8 (ByteString, pack)

-- For cryptonite
import qualified Crypto.PubKey.RSA as RSA
import qualified Crypto.PubKey.RSA.OAEP as OAEP
import qualified Crypto.Hash.Algorithms as HASH
import qualified Crypto.Random.Types as CRT

import Data.Bits (shiftL)

-- Helpers for RSA encryption
genKeyPair :: CRT.MonadRandom m => m (RSA.PublicKey, RSA.PrivateKey)
genKeyPair = RSA.generate 512 65537

encryptRSA :: CRT.MonadRandom m => RSA.PublicKey -> Bool -> m ByteString
encryptRSA p a = do
  let bs = boolToByteString a
  x <- OAEP.encrypt (OAEP.defaultOAEPParams HASH.SHA1) p bs
  case x of
    Left _ -> undefined
    Right b -> return b

decryptRSA :: CRT.MonadRandom m => RSA.PrivateKey -> ByteString -> m Bool
decryptRSA r bs = do
  x <- OAEP.decryptSafer (OAEP.defaultOAEPParams HASH.SHA1) r bs
  case x of
    Left _ -> undefined
    Right b -> return $ byteStringToBool b

boolToByteString :: Bool -> BS.StrictByteString
boolToByteString = pack . show

byteStringToBool :: BS.StrictByteString -> Bool
byteStringToBool bs
    | bs == pack (show True)  = True
    | bs == pack (show False) = False
    | otherwise             = undefined

generateFakePK :: CRT.MonadRandom m => m RSA.PublicKey
generateFakePK = do
    bytes <- CRT.getRandomBytes 512
    return $ RSA.PublicKey 512 (bytesToInteger bytes) 65537
      where bytesToInteger bs = foldl (\acc byte -> (acc `shiftL` 8) + fromIntegral byte) 0 (BS.unpack bs)

-- Multiple servers
-- Multiple clients
$(mkLoc "client1")
$(mkLoc "client2")
$(mkLoc "client3")

ot2Insecure :: (MonadIO m) =>
  Located '["client1"] Bool ->  -- sender
  Located '["client1"] Bool ->  -- sender
  Located '["client2"] Bool ->  -- receiver
  Choreo '["client1", "client2"] (CLI m) (Located '["client2"] Bool)
ot2Insecure b1 b2 s = do
  sr <- (client2 `introAnd` client2, s) ~> client1 @@ nobody
  (client1, \un -> return $ un client1 $ if (un client1 sr) then b1 else b2) ~~> client2 @@ nobody

-- Generate keys for OT, only one has a SK and the rest are fake
genKeys :: (CRT.MonadRandom m) => Bool -> m (RSA.PublicKey, RSA.PublicKey, RSA.PrivateKey)
genKeys s = do
  (pk, sk) <- genKeyPair
  fakePk <- generateFakePK
  return $ if s then (pk, fakePk, sk) else (fakePk, pk, sk)

-- Encryption based on select bit
enc2 :: (CRT.MonadRandom m) =>
       (RSA.PublicKey, RSA.PublicKey) -> Bool -> Bool ->
       m (ByteString, ByteString)
enc2 (pk1, pk2) b1 b2 = do
  c1 <- encryptRSA pk1 b1
  c2 <- encryptRSA pk2 b2
  return (c1, c2)

-- Decryption based on select bit
dec2 :: (CRT.MonadRandom m) =>
       (RSA.PublicKey, RSA.PublicKey, RSA.PrivateKey) ->
       Bool -> (ByteString, ByteString) ->
       m Bool
dec2 (_, _, sk) s (c1, c2) = do
  m <- if s then decryptRSA sk c1 else decryptRSA sk c2
  return m

-- One out of two OT
ot2 :: (KnownSymbol sender, KnownSymbol receiver, MonadIO m, CRT.MonadRandom m) =>
  Located '[sender] Bool ->  -- sender
  Located '[sender] Bool ->  -- sender
  Located '[receiver] Bool ->  -- receiver
  Choreo '[sender, receiver] (CLI m) (Located '[receiver] Bool)
ot2 b1 b2 s = do
  let sender = explicitMember :: Member sender '[sender, receiver]
  let receiver = (inSuper (consSuper refl) explicitMember) :: Member receiver '[sender, receiver]

  keys <- receiver `locally` \un -> (liftIO $ genKeys $ un explicitMember s)
  pks <- (receiver, \un -> let (pk1, pk2, _) = (un explicitMember keys) in return (pk1, pk2)) ~~> sender @@ nobody
  encrypted <- (sender, \un -> liftIO $ enc2 (un explicitMember pks)
                                             (un explicitMember b1)
                                             (un explicitMember b2)) ~~> receiver @@ nobody
  decrypted <- receiver `locally` \un -> liftIO $ dec2 (un explicitMember keys)
                                                       (un explicitMember s)
                                                       (un explicitMember encrypted)
  return decrypted

-- Test function
otTest :: (KnownSymbol p1, KnownSymbol p2, MonadIO m, CRT.MonadRandom m) => Choreo '[p1, p2] (CLI m) ()
otTest = do
  let p1 = explicitMember :: Member p1 '[p1, p2]
  let p2 = (inSuper (consSuper refl) explicitMember) :: Member p2 '[p1, p2]
  b1 <- p1 `_locally` return False
  b2 <- p1 `_locally` return True
  s <- p2 `_locally` return False
  otResult <- ot2 b1 b2 s
  p2 `locally_` \un -> putOutput "OT output:" $ un explicitMember otResult



main :: IO ()
main = do
  (pk, sk) <- genKeyPair
  encrypted <- encryptRSA pk True
  decrypted <- decryptRSA sk encrypted
  fakePK <- generateFakePK
  encrypted2 <- encryptRSA fakePK True
  putStrLn $ "pk:" ++ show pk
  putStrLn $ "Enc:" ++ show encrypted
  putStrLn $ "Dec:" ++ show decrypted
  putStrLn $ "Enc2:" ++ show encrypted2
  [loc] <- getArgs
  delivery <- case loc of
    "client1" -> runCLIIO $ runChoreography cfg (otTest @"client1" @"client2") "client1"
    "client2" -> runCLIIO $ runChoreography cfg (otTest @"client1" @"client2") "client2"
    _ -> error "unknown party"
  print delivery
  where
    cfg = mkHttpConfig [ ("client1", ("localhost", 4242))
                       , ("client2", ("localhost", 4343))
                       ]
