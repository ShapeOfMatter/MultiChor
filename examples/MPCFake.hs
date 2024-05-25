{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}


module MPCFake where

import Choreography
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


secretShare :: (KnownSymbols parties, KnownSymbol p, Wrapped w, MonadIO m)
            => Subset parties ps
            -> Member p ps
            -> (Member p owners, w owners Bool)
            -> Choreo ps m (Faceted parties Bool)
secretShare parties p (ownership, value) = do
  shares <- p `locally` \un -> do (freeShares :: [Bool]) <- case partyNames of
                                                              [] -> return [] -- This can't actually happen/get used...
                                                              _:others -> replicateM (length others) $ liftIO randomIO
                                  let lastShare = xor (un ownership value : freeShares)  -- But freeShares could really be empty!
                                  return $ partyNames `zip` (lastShare : freeShares)
  parties `fanOut` \q -> do
    share <- (p @@ nobody) `congruently` \un -> fromJust $ toLocTm q `lookup` un explicitSubset shares
    (p, (explicitMember, share)) ~> inSuper parties q @@ nobody
  where partyNames = toLocs parties

reveal :: (KnownSymbols ps)
       => Faceted ps Bool
       -> Choreo ps m Bool
reveal shares = do
  allShares <- fanIn refl refl \p -> (p, (explicitMember, localize p shares)) ~> refl
  value <- refl `congruently` \un -> case un refl allShares of [] -> error "There's nobody who can hit this"
                                                               aS -> xor aS
  naked refl value

computeWire :: (KnownSymbols ps, KnownSymbols parties, KnownSymbol trustedAnd, MonadIO m)
            => Member trustedAnd ps
            -> Subset parties ps
            -> Circuit parties
            -> Choreo ps (CLI m) (Faceted parties Bool)
computeWire trustedAnd parties circuit = case circuit of
  InputWire p -> do
    value <- inSuper parties p `_locally` getInput "Enter a secret input value:"
    secretShare parties (inSuper parties p) (explicitMember, value)
  LitWire b -> do
    let shares = partyNames `zip` (b : repeat False)
    parties `fanOut` \p -> inSuper parties p `_locally` return (fromJust $ toLocTm p `lookup` shares)
  AndGate l r -> do
    lResult <- compute l
    rResult <- compute r
    inputShares <- fanIn parties (trustedAnd @@ nobody) \p -> do
      (inSuper parties p, \un -> return (un p lResult, un p rResult)) ~~> trustedAnd @@ nobody
    outputVal <- (trustedAnd @@ nobody) `congruently` \un ->
      let ovs = un refl inputShares
      in case ovs of [] -> error "make sure there's at least one party"
                     _:_ -> xor (fst <$> un refl inputShares) && xor (snd <$> un refl inputShares)
    secretShare parties trustedAnd (explicitMember, outputVal)
  XorGate l r -> do
    lResult <- compute l
    rResult <- compute r
    parties `parallel` \p un -> return (un p lResult /= un p rResult)
  where compute = computeWire trustedAnd parties
        partyNames = toLocs parties

mpc :: (KnownSymbols parties, MonadIO m)
    => Circuit parties
    -> Choreo ("trusted3rdParty" ': parties) (CLI m) ()
mpc circuit = do
  let parties = consSuper refl
  outputWire <- computeWire trusted3rdParty parties circuit
  result <- enclave parties $ reveal outputWire
  void $ parties `parallel` \p un -> putOutput "The resulting bit:" $ un p result

