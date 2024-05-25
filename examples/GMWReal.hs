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
import Logic.Propositional (introAnd)
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


secretShare :: forall parties p m.
  (KnownSymbols parties, KnownSymbol p, MonadIO m)
            => Member p parties
            -> Located '[p] Bool
            -> Choreo parties m (Faceted parties Bool)
secretShare p value = do
  shares <- p `locally` \un -> do (freeShares :: [Bool]) <- case partyNames of
                                                              [] -> return [] -- This can't actually happen/get used...
                                                              _:others -> replicateM (length others) $ liftIO randomIO
                                  let lastShare = xor (un explicitMember value : freeShares)  -- But freeShares could really be empty!
                                  return $ partyNames `zip` (lastShare : freeShares)
  refl `fanOut` \q -> do
    share <- (p @@ nobody) `congruently` \un -> fromJust $ toLocTm q `lookup` un explicitSubset shares
    (p, (explicitMember, share)) ~> q @@ nobody
  where partyNames = toLocs @parties refl

reveal :: (KnownSymbols ps)
       => Faceted ps Bool
       -> Choreo ps m Bool
reveal shares = do
  allShares <- fanIn refl refl \p -> (p, (explicitMember, localize p shares)) ~> refl
  value <- refl `congruently` \un -> case un refl allShares of [] -> error "There's nobody who can hit this"
                                                               aS -> xor aS
  naked refl value

genBools :: (MonadIO m) => [LocTm] -> CLI m [(LocTm, Bool)]
genBools names = mapM genBool names
  where genBool n = do r <- randomRIO (0, 1 :: Int); return $ (n, r == 0)

-- use OT to do multiplication
fMult :: forall parties m.
  (KnownSymbols parties, MonadIO m, CRT.MonadRandom m)
      => Faceted parties Bool
      -> Faceted parties Bool
      -> Choreo parties (CLI m) (Faceted parties Bool)
fMult u_shares v_shares = do
  let party_names = toLocs @parties refl
  a_ij_s :: Faceted parties [(LocTm, Bool)] <- refl `parallel` \_ _ -> genBools party_names
  b_ij_s :: Faceted parties Bool <- refl `fanOut` (fMultOne a_ij_s u_shares v_shares)
  ind_names :: Faceted parties LocTm <- refl `fanOut` \p_i -> p_i `_locally` return (toLocTm p_i)
  new_shares :: Faceted parties Bool <- refl `parallel` \p_i un -> return (computeShare
                                                                           (un p_i ind_names)
                                                                           (un p_i u_shares)
                                                                           (un p_i v_shares)
                                                                           (un p_i a_ij_s)
                                                                           (un p_i b_ij_s))
  return new_shares

computeShare :: LocTm -> Bool -> Bool
             -> [(LocTm, Bool)]
             -> Bool -> Bool
computeShare p_i u_i v_i a_ij b = xor $ [u_i && v_i, b] ++ (map snd $ filter ok a_ij)
  where ok (p_j, _) = p_j /= p_i

-- use OT to do multiplication, for party p_j
fMultOne :: (KnownSymbols parties, KnownSymbol p_j, MonadIO m, CRT.MonadRandom m)
         => Faceted parties [(LocTm, Bool)]
         -> Faceted parties Bool
         -> Faceted parties Bool
         -> Member p_j parties
         -> Choreo parties (CLI m) (Located '[p_j] Bool)
fMultOne a_ij_s u_shares v_shares p_j = do
  b_jis :: Located '[p_j] [Bool] <- fanIn refl (p_j @@ nobody) (fMultBij a_ij_s u_shares v_shares p_j)
  sumShares :: Located '[p_j] Bool <- p_j `locally` \un -> return $ xor $ un explicitMember b_jis
  return sumShares

-- use OT to do multiplication, for party p_i and p_j
fMultBij :: (KnownSymbols parties, KnownSymbol p_i, KnownSymbol p_j, MonadIO m, CRT.MonadRandom m)
         => Faceted parties [(LocTm, Bool)]
         -> Faceted parties Bool
         -> Faceted parties Bool
         -> Member p_j parties
         -> Member p_i parties
         -> Choreo parties (CLI m) (Located '[p_j] Bool)
fMultBij a_ij_s u_shares v_shares p_j p_i = do
  let p_i_name = toLocTm p_i
      p_j_name = toLocTm p_j
  case p_i_name == p_j_name of
    True  -> p_j `_locally` return False
    False -> do
      a_ij :: Located '[p_i] Bool <- p_i `locally` \un -> return $ fromJust $ lookup p_j_name (un p_i a_ij_s)
      u_i :: Located '[p_i] Bool <- p_i `locally` \un -> return (un p_i u_shares)
      b1 :: Located '[p_i] Bool <- p_i `locally` \un -> return $ (un explicitMember u_i) /= (un explicitMember a_ij)
      b2 :: Located '[p_i] Bool <- p_i `locally` \un -> return $ un explicitMember a_ij
      select_bit :: Located '[p_j] Bool <- p_j `locally` \un -> return (un p_j v_shares)
      b_ij_w :: Located '[p_i, p_j] (Located '[p_j] Bool) <- (p_i @@ p_j @@ nobody) `enclave` (ot2 b1 b2 select_bit)
      let b_ij :: Located '[p_j] Bool = (consSet `introAnd` refl) `flatten` b_ij_w
      return b_ij


computeWire :: forall parties m.
               (KnownSymbols parties, MonadIO m, CRT.MonadRandom m)
            => Circuit parties
            -> Choreo parties (CLI m) (Faceted parties Bool)
computeWire circuit = case circuit of
  InputWire p -> do
    value :: Located '[p] Bool <- p `_locally` getInput "Enter a secret input value:"
    secretShare p value
  LitWire b -> do
    let shares = partyNames `zip` (b : repeat False)
    refl `fanOut` \p -> p `_locally` return (fromJust $ toLocTm p `lookup` shares)
  AndGate l r -> do
    lResult <- computeWire l
    rResult <- computeWire r
    fMult lResult rResult
  XorGate l r -> do
    lResult <- computeWire l
    rResult <- computeWire r
    refl `parallel` \p un -> return $ xor [un p lResult, un p rResult]
  where partyNames = toLocs @parties refl

mpc :: (KnownSymbols parties, MonadIO m, CRT.MonadRandom m)
    => Circuit parties
    -> Choreo parties (CLI m) ()
mpc circuit = do
  outputWire <- computeWire circuit
  result <- reveal outputWire
  void $ refl `parallel` \_ _ -> putOutput "The resulting bit:" $ result

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
