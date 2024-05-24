{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module ObliviousTransfer (ot2, ot4, main) where

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
genKeyPair = RSA.generate 64 65537

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
    bytes <- CRT.getRandomBytes 64
    return $ RSA.PublicKey 64 (bytesToInteger bytes) 65537
      where bytesToInteger bs = foldl (\acc byte -> (acc `shiftL` 8) + fromIntegral byte) 0 (BS.unpack bs)


--------------------------------------------------
-- 1-out-of-2 Oblivious transfer
--------------------------------------------------
ot2Insecure :: (KnownSymbol sender, KnownSymbol receiver, MonadIO m) =>
  Located '[sender] Bool ->
  Located '[sender] Bool ->
  Located '[receiver] Bool ->
  Choreo '[sender, receiver] (CLI m) (Located '[receiver] Bool)
ot2Insecure b1 b2 s = do
  let sender = explicitMember :: Member sender '[sender, receiver]
  let receiver = (inSuper (consSuper refl) explicitMember) :: Member receiver '[sender, receiver]
  sr <- (explicitMember `introAnd` receiver, s) ~> sender @@ nobody
  (sender, \un -> return $ un explicitMember $ if (un explicitMember sr) then b1 else b2) ~~> receiver @@ nobody

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

--------------------------------------------------
-- 1-out-of-4 Oblivious transfer
--------------------------------------------------

select4 :: Bool -> Bool -> a -> a -> a -> a -> a
select4 s1 s2 v1 v2 v3 v4 = case (s1, s2) of
  (True,  True)  -> v1
  (True,  False) -> v2
  (False, True)  -> v3
  (False, False) -> v4

ot4Insecure :: (KnownSymbol sender, KnownSymbol receiver, MonadIO m) =>
  Located '[sender] Bool ->  -- sender
  Located '[sender] Bool ->  -- sender
  Located '[sender] Bool ->  -- sender
  Located '[sender] Bool ->  -- sender
  Located '[receiver] Bool ->  -- receiver
  Located '[receiver] Bool ->  -- receiver
  Choreo '[sender, receiver] (CLI m) (Located '[receiver] Bool)
ot4Insecure b1 b2 b3 b4 s1 s2 = do
  let sender = explicitMember :: Member sender '[sender, receiver]
  let receiver = (inSuper (consSuper refl) explicitMember) :: Member receiver '[sender, receiver]

  s1r <- (explicitMember `introAnd` receiver, s1) ~> sender @@ nobody
  s2r <- (explicitMember `introAnd` receiver, s2) ~> sender @@ nobody
  (sender, \un -> return $ un explicitMember $ select4 (un explicitMember s1r) (un explicitMember s2r) b1 b2 b3 b4) ~~> receiver @@ nobody

-- Generate keys for OT, only one has a SK and the rest are fake
genKeys4 :: (CRT.MonadRandom m) =>
            Bool -> Bool ->
            m (RSA.PublicKey, RSA.PublicKey, RSA.PublicKey, RSA.PublicKey, RSA.PrivateKey)
genKeys4 s1 s2 = do
  (pk, sk) <- genKeyPair
  fakePk1 <- generateFakePK
  fakePk2 <- generateFakePK
  fakePk3 <- generateFakePK
  return $ case (s1, s2) of
    (True,  True)  -> (pk,      fakePk1, fakePk2, fakePk3, sk)
    (True,  False) -> (fakePk1, pk,      fakePk2, fakePk3, sk)
    (False, True)  -> (fakePk1, fakePk2, pk,      fakePk3, sk)
    (False, False) -> (fakePk1, fakePk2, fakePk3, pk,      sk)


-- Encryption based on select bit
enc4 :: (CRT.MonadRandom m) =>
       (RSA.PublicKey, RSA.PublicKey, RSA.PublicKey, RSA.PublicKey) ->
       Bool -> Bool -> Bool -> Bool ->
       m (ByteString, ByteString, ByteString, ByteString)
enc4 (pk1, pk2, pk3, pk4) b1 b2 b3 b4 = do
  c1 <- encryptRSA pk1 b1
  c2 <- encryptRSA pk2 b2
  c3 <- encryptRSA pk3 b3
  c4 <- encryptRSA pk4 b4
  return (c1, c2, c3, c4)

-- Decryption based on select bit
dec4 :: (CRT.MonadRandom m) =>
       (RSA.PublicKey, RSA.PublicKey, RSA.PublicKey, RSA.PublicKey, RSA.PrivateKey) ->
       Bool -> Bool -> (ByteString, ByteString, ByteString, ByteString) ->
       m Bool
dec4 (_, _, _, _, sk) s1 s2 (c1, c2, c3, c4) = decryptRSA sk $ select4 s1 s2 c1 c2 c3 c4

-- One out of two OT
ot4 :: (KnownSymbol sender, KnownSymbol receiver, MonadIO m, CRT.MonadRandom m) =>
  Located '[sender] Bool ->
  Located '[sender] Bool ->
  Located '[sender] Bool ->
  Located '[sender] Bool ->
  Located '[receiver] Bool ->
  Located '[receiver] Bool ->
  Choreo '[sender, receiver] (CLI m) (Located '[receiver] Bool)
ot4 b1 b2 b3 b4 s1 s2 = do
  let sender = explicitMember :: Member sender '[sender, receiver]
  let receiver = (inSuper (consSuper refl) explicitMember) :: Member receiver '[sender, receiver]

  keys <- receiver `locally` \un -> (liftIO $ genKeys4 (un explicitMember s1) (un explicitMember s2))
  pks <- (receiver, \un -> let (pk1, pk2, pk3, pk4, _) = (un explicitMember keys) in return (pk1, pk2, pk3, pk4)) ~~> sender @@ nobody
  encrypted <- (sender, \un -> liftIO $ enc4 (un explicitMember pks)
                                             (un explicitMember b1)
                                             (un explicitMember b2)
                                             (un explicitMember b3)
                                             (un explicitMember b4)
               ) ~~> receiver @@ nobody
  decrypted <- receiver `locally` \un -> liftIO $ dec4 (un explicitMember keys)
                                                       (un explicitMember s1)
                                                       (un explicitMember s2)
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
  otResultI <- ot2Insecure b1 b2 s
  p2 `locally_` \un -> putOutput "OT2 insecure output:" $ un explicitMember otResultI
  otResult <- ot2 b1 b2 s
  p2 `locally_` \un -> putOutput "OT2 output:" $ un explicitMember otResult

  b3 <- p1 `_locally` return False
  b4 <- p1 `_locally` return True
  s2 <- p2 `_locally` return False
  otResultI4 <- ot4Insecure b1 b2 b3 b4 s s2
  p2 `locally_` \un -> putOutput "OT4 insecure output:" $ un explicitMember otResultI4
  otResult4 <- ot4 b1 b2 b3 b4 s s2
  p2 `locally_` \un -> putOutput "OT4 output:" $ un explicitMember otResult4


main :: IO ()
main = do
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