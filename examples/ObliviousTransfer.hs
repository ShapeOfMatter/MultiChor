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
import Logic.Classes (Reflexive, refl, Transitive, transitive)
import qualified Data.ByteString as BS

import qualified Sel.PublicKey.Seal as Seal
import qualified Sel.PublicKey.Cipher as Cipher

import Data.ByteString.Char8 (ByteString, pack)
import qualified Data.ByteString.Lazy as BL
import Text.Read (Read(..), readPrec, lexP, parens)

import Data.Maybe (fromJust)
--import Text.ParserCombinators.ReadP (readP_to_Prec)

-- For cryptonite
import qualified Crypto.PubKey.RSA as RSA
import qualified Crypto.PubKey.RSA.OAEP as OAEP
import qualified Crypto.PubKey.RSA.PSS as PSS
import qualified Crypto.Hash.Algorithms as HASH
import qualified Crypto.Random.Types as CRT
import qualified Data.Text.Encoding as E
import           Data.ByteArray.Encoding as BAE

import Data.Bits (shiftL)
--import Data.Text

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

-- Multiple servers
-- Multiple clients
$(mkLoc "client1")
$(mkLoc "client2")
$(mkLoc "client3")

-- p2pSum :: (MonadIO m) => Choreo Participants (CLI m) ()
-- p2pSum = do
--   shares1 <- client1 `locally` \_ -> secretShare
--   shares2 <- client2 `locally` \_ -> secretShare
--   s12s <- (client1, \un -> return $ snd $ un client1 shares1) ~~> client2 @@ nobody
--   s21s <- (client2, \un -> return $ snd $ un client2 shares2) ~~> client1 @@ nobody
--   sum1 <- (client1, \un -> return $ (fst $ un client1 shares1) + (un client1 s21s)) ~~> client1 @@ client2 @@ nobody
--   sum2 <- (client2, \un -> return $ (un client2 s12s) + (fst $ un client2 shares2)) ~~> client1 @@ client2 @@ nobody
--   total1 <- client1 `locally` \un -> return $ (un client1 sum1) + (un client1 sum2)
--   total2 <- client2 `locally` \un -> return $ (un client2 sum1) + (un client2 sum2)
--   client1 `locally_` \un -> putOutput "Total:" $ un client1 total1
--   client2 `locally_` \un -> putOutput "Total:" $ un client2 total2


--ot :: (KnownSymbol p1, KnownSymbol p2, MonadIO m) => Choreo '[p1, p2] (CLI m) ()
-- ot :: (KnownSymbol p1, KnownSymbol p2, KnownSymbols ps, MonadIO m) => Member p1 ps -> Member p2 ps -> Choreo ps (CLI m) ()
ot2Insecure :: (MonadIO m) =>
  Located '["client1"] Bool ->  -- sender
  Located '["client1"] Bool ->  -- sender
  Located '["client2"] Bool ->  -- receiver
  Choreo '["client1", "client2"] (CLI m) (Located '["client2"] Bool)
ot2Insecure b1 b2 s = do
  sr <- (client2 `introAnd` client2, s) ~> client1 @@ nobody
  (client1, \un -> return $ un client1 $ if (un client1 sr) then b1 else b2) ~~> client2 @@ nobody

genKeys :: (CRT.MonadRandom m) => Bool -> m (RSA.PublicKey, RSA.PublicKey, RSA.PrivateKey)
genKeys s = do
  (pk, sk) <- genKeyPair
  fakePk <- generateFakePK
  return $ if s then (pk, fakePk, sk) else (fakePk, pk, sk)

enc :: (CRT.MonadRandom m) =>
       (RSA.PublicKey, RSA.PublicKey) -> Bool -> Bool ->
       m (ByteString, ByteString)
enc (pk1, pk2) b1 b2 = do
  c1 <- encryptRSA pk1 b1
  c2 <- encryptRSA pk2 b2
  return (c1, c2)

dec :: (CRT.MonadRandom m) =>
       (RSA.PublicKey, RSA.PublicKey, RSA.PrivateKey) ->
       Bool -> (ByteString, ByteString) ->
       m Bool
dec (_, _, sk) s (c1, c2) = do
  m <- if s then decryptRSA sk c1 else decryptRSA sk c2
  return m

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
  encrypted <- (sender, \un -> liftIO $ enc (un explicitMember pks)
                                            (un explicitMember b1)
                                            (un explicitMember b2)) ~~> receiver @@ nobody
  decrypted <- receiver `locally` \un -> liftIO $ dec (un explicitMember keys)
                                                      (un explicitMember s)
                                                      (un explicitMember encrypted)
  return decrypted

dec2 :: Bool ->
  (Cipher.PublicKey, Cipher.SecretKey) ->
  (Cipher.PublicKey, Cipher.SecretKey) ->
  (BS.StrictByteString, BS.StrictByteString) -> Bool
dec2 s (pk1, sk1) (pk2, sk2) (cb1, cb2) =
  case (Cipher.cipherTextFromHexByteString cb1,
        Cipher.cipherTextFromHexByteString cb2) of
    (Right c1, Right c2) -> byteStringToBool $ fromJust $ if s then Seal.open c1 pk1 sk1 else Seal.open c2 pk2 sk2
    (_, _) -> undefined


enc2 :: (MonadIO m) => (BS.StrictByteString, BS.StrictByteString) -> Bool -> Bool ->
  CLI m (BS.StrictByteString, BS.StrictByteString)
enc2 (pk1, pk2) b1 b2 = do
  putOutput "OT output:" $ b1
  c1 <- liftIO $ Seal.seal (boolToByteString b1) (getPK pk1)
  c2 <- liftIO $ Seal.seal (boolToByteString b2) (getPK pk2)
  return (Cipher.cipherTextToHexByteString c1,
          Cipher.cipherTextToHexByteString c2)

-- instance Read Cipher.CipherText where
--   readsPrec = readP_to_Prec $ \_ -> do
--     str <- lexP
--     let bs = BS.pack str
--     case Cipher.cipherTextFromHexByteString bs of
--       Left _ -> undefined
--       Right c -> return (c, "")

getPK :: BS.StrictByteString -> Cipher.PublicKey
getPK pkb = let skb = BS.empty in
  case Cipher.keyPairFromHexByteStrings pkb skb of
    Left _ -> undefined
    Right (pk, _) -> pk

getSK :: BS.StrictByteString -> Cipher.SecretKey
getSK skb = let pkb = BS.empty in
  case Cipher.keyPairFromHexByteStrings pkb skb of
    Left _ -> undefined
    Right (_, sk) -> sk

otTest :: (KnownSymbol p1, KnownSymbol p2, MonadIO m, CRT.MonadRandom m) => Choreo '[p1, p2] (CLI m) ()
otTest = do
  let p1 = explicitMember :: Member p1 '[p1, p2]
  let p2 = (inSuper (consSuper refl) explicitMember) :: Member p2 '[p1, p2]
  b1 <- p1 `_locally` return False
  b2 <- p1 `_locally` return True
  s <- p2 `_locally` return False
  otResult <- ot2 b1 b2 s
  p2 `locally_` \un -> putOutput "OT output:" $ un explicitMember otResult

boolToByte :: Bool -> BL.ByteString
boolToByte True  = BL.pack [1]  -- Represent True as byte 1
boolToByte False = BL.pack [0]  -- Represent False as byte 0

-- Function to generate a 512-bit integer
generateFakePK :: CRT.MonadRandom m => m RSA.PublicKey
generateFakePK = do
    bytes <- CRT.getRandomBytes 512
    return $ RSA.PublicKey 512 (bytesToInteger bytes) 65537
      where bytesToInteger bs = foldl (\acc byte -> (acc `shiftL` 8) + fromIntegral byte) 0 (BS.unpack bs)


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
