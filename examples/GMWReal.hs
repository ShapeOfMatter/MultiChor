{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module GMWReal where

import System.Environment

import Choreography
import Choreography.Network.Http
import CLI
import Control.Monad (replicateM, void)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data (TestArgs, reference)
import Data.Kind (Type)
import Data.Maybe (fromJust)
import GHC.TypeLits (KnownSymbol)
import Logic.Classes (refl)
import System.Random
import Test.QuickCheck (Arbitrary, arbitrary, chooseInt, elements, getSize, oneof, resize)

import ObliviousTransfer
import qualified Crypto.Random.Types as CRT

$(mkLoc "trusted3rdParty")
$(mkLoc "p1")
$(mkLoc "p2")
$(mkLoc "p3")
$(mkLoc "p4")


xor :: [Bool] -> Bool
xor [] = error "don't let this happen!"
xor bs = foldr1 (/=) bs

data Circuit :: [LocTy] -> Type where
  InputWire :: (KnownSymbol p) => Member p ps -> Circuit ps
  LitWire :: Bool -> Circuit ps
  AndGate :: Circuit ps -> Circuit ps -> Circuit ps
  XorGate :: Circuit ps -> Circuit ps -> Circuit ps

instance Show (Circuit ps) where
  show (InputWire p) = "InputWire<" ++ toLocTm p ++ ">"
  show (LitWire b) = "LitWire " ++ show b
  show (AndGate left right) = "(" ++ show left ++ ") AND (" ++ show right ++ ")"
  show (XorGate left right) = "(" ++ show left ++ ") XOR (" ++ show right ++ ")"

instance Arbitrary (Circuit '["p1", "p2", "p3", "p4"]) where
  arbitrary = do size <- getSize
                 if 1 >= size
                   then oneof $ (LitWire <$> arbitrary) : (return <$> [InputWire p1, InputWire p2, InputWire p3, InputWire p4])
                   else do left <- chooseInt (1, size)
                           a <- resize left arbitrary
                           b <- resize (1 `max` (size - left)) arbitrary
                           op <- elements [AndGate, XorGate]
                           return $ a `op` b

data Args = Args{ circuit :: Circuit '["p1", "p2", "p3", "p4"]
                , p1in :: Bool  -- These should be lists, but consuming them would be a chore...
                , p2in :: Bool
                , p3in :: Bool
                , p4in :: Bool
                } deriving (Show)
instance Arbitrary Args where
  arbitrary = Args <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
instance TestArgs Args (Bool, Bool, Bool, Bool) where
  reference Args{circuit, p1in, p2in, p3in, p4in} = (answer, answer, answer, answer)
    where recurse c = case c of
            InputWire p -> fromJust $ toLocTm p `lookup` inputs
            LitWire b -> b
            AndGate left right -> recurse left && recurse right
            XorGate left right -> recurse left /= recurse right
          inputs = ["p1", "p2", "p3", "p4"] `zip` [p1in, p2in, p3in, p4in]
          answer = recurse circuit


genShares :: (MonadIO m) => [LocTm] -> Bool -> m [(LocTm, Bool)]
genShares parties x = do
  freeShares <- replicateM (length parties - 1) $ liftIO randomIO -- generate n-1 random shares
  return $ zip parties $ xor (x : freeShares) : freeShares        -- make the sum equal to x

secretShare :: forall parties p m. (KnownSymbols parties, KnownSymbol p, MonadIO m)
            => Member p parties -> Located '[p] Bool -> Choreo parties m (Faceted parties Bool)
secretShare p value = do
  shares <- p `locally` \un -> genShares partyNames (un singleton value)
  refl `fanOut` \q ->
    (p, \un -> return $ fromJust $ toLocTm q `lookup` un singleton shares) ~~> q @@ nobody
  where partyNames = toLocs @parties refl

reveal :: (KnownSymbols ps)
       => Faceted ps Bool
       -> Choreo ps m Bool
reveal shares = do
  allShares <- fanIn refl refl \p -> (p, (p, shares)) ~> refl
  value <- refl `congruently` \un -> case un refl allShares of [] -> error "There's nobody who can hit this"
                                                               aS -> xor aS
  naked refl value

genBools :: (MonadIO m) => [LocTm] -> CLI m [(LocTm, Bool)]
genBools names = mapM genBool names
  where genBool n = do r <- randomRIO (0, 1 :: Int); return $ (n, r == 0)

-- use OT to do multiplication
fAnd :: forall parties m. (KnownSymbols parties, MonadIO m, CRT.MonadRandom m)
     => Faceted parties Bool -> Faceted parties Bool -> Choreo parties (CLI m) (Faceted parties Bool)
fAnd u_shares v_shares = do
  let party_names = toLocs @parties refl
  a_ij_s  :: Faceted parties [(LocTm, Bool)] <- refl `_parallel` genBools party_names
  b_ij_s  :: Faceted parties Bool  <- refl `fanOut` (fAndAij a_ij_s u_shares v_shares)
  names   :: Faceted parties LocTm <- refl `fanOut` \p_i -> p_i `_locally` return (toLocTm p_i)
  shares' :: Faceted parties Bool  <- refl `parallel` (computeShare names u_shares v_shares a_ij_s b_ij_s)
  return shares'

fAndAij :: (KnownSymbols parties, KnownSymbol p_j, MonadIO m, CRT.MonadRandom m)
        => Faceted parties [(LocTm, Bool)] -> Faceted parties Bool -> Faceted parties Bool
        -> Member p_j parties -> Choreo parties (CLI m) (Located '[p_j] Bool)
fAndAij a_ij_s u_shares v_shares p_j = do
  b_jis :: Located '[p_j] [Bool] <- fanIn refl (p_j @@ nobody) (fAndBij a_ij_s u_shares v_shares p_j)
  sum   :: Located '[p_j] Bool   <- p_j `locally` \un -> return $ xor $ un singleton b_jis
  return sum

fAndBij :: (KnownSymbols parties, KnownSymbol p_i, KnownSymbol p_j, MonadIO m, CRT.MonadRandom m)
        => Faceted parties [(LocTm, Bool)] -> Faceted parties Bool -> Faceted parties Bool
        -> Member p_j parties -> Member p_i parties -> Choreo parties (CLI m) (Located '[p_j] Bool)
fAndBij a_ij_s u_shares v_shares p_j p_i = do
  let p_i_name = toLocTm p_i
      p_j_name = toLocTm p_j
  case p_i_name == p_j_name of
    True  -> p_j `_locally` return False
    False -> do
      a_ij :: Located '[p_i] Bool <- p_i `locally` \un -> return $ fromJust $ lookup p_j_name (un p_i a_ij_s)
      u_i  :: Located '[p_i] Bool <- p_i `locally` \un -> return (un p_i u_shares)
      b1   :: Located '[p_i] Bool <- p_i `locally` \un -> return $ xor [(un singleton u_i), (un singleton a_ij)]
      b2   :: Located '[p_i] Bool <- p_i `locally` \un -> return $ un singleton a_ij
      s    :: Located '[p_j] Bool <- p_j `locally` \un -> return (un p_j v_shares)
      b_ij :: Located '[p_j] Bool <- enclaveTo (p_i @@ p_j @@ nobody) (listedSecond @@ nobody) (ot2 b1 b2 s)
      return b_ij

computeShare :: (MonadIO m)
             => Faceted parties LocTm -> Faceted parties Bool -> Faceted parties Bool
             -> Faceted parties [(LocTm, Bool)] -> Faceted parties Bool -> Member p parties
             -> Unwrap p -> m Bool
computeShare ind_names u_shares v_shares a_ij_s b_ij_s p_i un =
  return (computeShareL
           (un p_i ind_names)
           (un p_i u_shares)
           (un p_i v_shares)
           (un p_i a_ij_s)
           (un p_i b_ij_s))
  where computeShareL p_i u_i v_i a_ij b = xor $ [u_i && v_i, b] ++ (map snd $ filter (ok p_i) a_ij)
        ok p_i (p_j, _) = p_j /= p_i


gmw :: forall parties m. (KnownSymbols parties, MonadIO m, CRT.MonadRandom m)
    => Circuit parties -> Choreo parties (CLI m) (Faceted parties Bool)
gmw circuit = case circuit of
  InputWire p -> do        -- process a secret input value from party p
    value :: Located '[p] Bool <- p `_locally` getInput "Enter a secret input value:"
    secretShare p value
  LitWire b ->             -- process a publicly-known literal value
    let shares = partyNames `zip` (b : repeat False) in
    refl `fanOut` \p -> p `_locally` return (fromJust $ toLocTm p `lookup` shares)
  AndGate l r -> do        -- process an AND gate
    lResult <- gmw l; rResult <- gmw r;
    fAnd lResult rResult
  XorGate l r -> do        -- process an XOR gate
    lResult <- gmw l; rResult <- gmw r
    refl `parallel` \p un -> return $ xor [un p lResult, un p rResult]
  where partyNames = toLocs @parties refl

mpc :: (KnownSymbols parties, MonadIO m, CRT.MonadRandom m)
    => Circuit parties
    -> Choreo parties (CLI m) ()
mpc circuit = do
  outputWire <- gmw circuit
  result <- reveal outputWire
  void $ refl `_parallel` putOutput "The resulting bit:" result

mpcmany :: (KnownSymbols parties, MonadIO m, CRT.MonadRandom m)
    => Circuit parties
    -> Choreo parties (CLI m) ()
mpcmany circuit = do
  mpc circuit

type Clients = '["p1", "p2"]--, "p3", "p4"]
main :: IO ()
main = do
  let circuit :: Circuit Clients = (AndGate (LitWire True) (LitWire True))
  [loc] <- getArgs
  delivery <- case loc of
    "p1" -> runCLIIO $ runChoreography cfg (mpcmany @Clients circuit) "p1"
    "p2" -> runCLIIO $ runChoreography cfg (mpcmany @Clients circuit) "p2"
--    "p3" -> runCLIIO $ runChoreography cfg (mpcmany @Clients circuit) "p3"
--    "p4" -> runCLIIO $ runChoreography cfg (mpcmany @Clients circuit) "p4"
    _ -> error "unknown party"
  print delivery
  where
    cfg = mkHttpConfig [ ("p1", ("localhost", 4242))
                       , ("p2", ("localhost", 4343))
--                       , ("p3", ("localhost", 4344))
--                       , ("p4", ("localhost", 4345))
                       ]
