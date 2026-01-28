{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module MPCFake where

import CLI
import Choreography
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data (TestArgs, reference)
import Data.Kind (Type)
import Data.Known.Knowable
import Data.Maybe (fromJust)
import Data.Known.Membership
import Data.Known.TypeIndexed
import EasyMain (easyMain)
import System.Random
import Test.QuickCheck (Arbitrary, arbitrary, chooseInt, elements, getSize, oneof, resize)

trusted3rdParty :: forall ps. ExplicitMember "trusted3rdParty" ps => Member "trusted3rdParty" ps
trusted3rdParty = explicitMember
p1 :: forall ps. ExplicitMember "p1" ps => Member "p1" ps
p1 = explicitMember
p2 :: forall ps. ExplicitMember "p2" ps => Member "p2" ps
p2 = explicitMember
p3 :: forall ps. ExplicitMember "p3" ps => Member "p3" ps
p3 = explicitMember
p4 :: forall ps. ExplicitMember "p4" ps => Member "p4" ps
p4 = explicitMember

xor :: (Foldable f) => f Bool -> Bool
xor = foldr1 (/=)

data Circuit :: [Party] -> Type where
  InputWire :: (Known Party p) => Member p ps -> Circuit ps
  LitWire :: Bool -> Circuit ps
  AndGate :: Circuit ps -> Circuit ps -> Circuit ps
  XorGate :: Circuit ps -> Circuit ps -> Circuit ps

instance Show (Circuit ps) where
  show (InputWire p) = "InputWire<" ++ toValue p ++ ">"
  show (LitWire b) = "LitWire " ++ show b
  show (AndGate left right) = "(" ++ show left ++ ") AND (" ++ show right ++ ")"
  show (XorGate left right) = "(" ++ show left ++ ") XOR (" ++ show right ++ ")"

instance Arbitrary (Circuit '["p1", "p2", "p3", "p4"]) where
  arbitrary = do
    size <- getSize
    if 1 >= size
      then oneof $ (LitWire <$> arbitrary) : (return <$> [InputWire p1, InputWire p2, InputWire p3, InputWire p4])
      else do
        left <- chooseInt (1, size)
        a <- resize left arbitrary
        b <- resize (1 `max` (size - left)) arbitrary
        op <- elements [AndGate, XorGate]
        return $ a `op` b

data Args = Args
  { circuit :: Circuit '["p1", "p2", "p3", "p4"],
    p1in :: Bool, -- These should be lists, but consuming them would be a chore...
    p2in :: Bool,
    p3in :: Bool,
    p4in :: Bool
  }
  deriving (Show)

instance Arbitrary Args where
  arbitrary = Args <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance TestArgs Args (Bool, Bool, Bool, Bool) where
  reference Args {circuit, p1in, p2in, p3in, p4in} = (answer, answer, answer, answer)
    where
      recurse c = case c of
        InputWire p -> fromJust $ toValue p `lookup` inputs
        LitWire b -> b
        AndGate left right -> recurse left && recurse right
        XorGate left right -> recurse left /= recurse right
      inputs = ["p1", "p2", "p3", "p4"] `zip` [p1in, p2in, p3in, p4in]
      answer = recurse circuit

secretShare ::
  forall p parties owners ps m.
  (Known [Party] parties, Known Party p, MonadIO m) =>
  Subset parties ps ->
  Member p ps ->
  (Member p owners, Located owners Bool) ->
  Choreo ps m (Faceted parties '[] Bool)
secretShare parties p (ownership, value) = do
  shares <- p `locally` \un -> genShares (un ownership value)
  PIndexed fs <- scatter p parties shares
  return $ PIndexed $ Facet . othersForget (First @@ nobody) . getFacet . fs
  where
    genShares x = case tySpine @Party @parties of
      TyCons _ _ -> gs'
      TyNil -> error "Can't secret-share to zero people."
      where
        gs' :: forall q qs. (Known Party q, Known [Party] qs) => m (Quire (q ': qs) Bool)
        gs' = do
          freeShares <- sequence $ pure $ liftIO randomIO -- generate n-1 random shares
          return $ xor (qCons @q x freeShares) `qCons` freeShares

reveal ::
  forall ps m.
  (Known [Party] ps) =>
  Faceted ps '[] Bool ->
  Choreo ps m Bool
reveal shares = do
  let ps = allOf @ps
  allShares <- gather ps ps shares
  value <- ps `congruently` \un -> xor $ un ps allShares
  naked ps value

computeWire ::
  (Known [Party] ps, Known [Party] parties, Known Party trustedAnd, MonadIO m) =>
  Member trustedAnd ps ->
  Subset parties ps ->
  Circuit parties ->
  Choreo ps (CLI m) (Faceted parties '[] Bool)
computeWire trustedAnd parties circuit = case circuit of
  InputWire p -> do
    value <- inSuper parties p `_locally` getInput "Enter a secret input value:"
    secretShare parties (inSuper parties p) (alone, value)
  LitWire b -> do
    let shares = partyNames `zip` (b : repeat False)
    fanOut \p -> inSuper parties p `_locally` return (fromJust $ toValue p `lookup` shares)
  AndGate l r -> do
    lResult <- compute l
    rResult <- compute r
    inputShares <- fanIn (trustedAnd @@ nobody) \p -> do
      (inSuper parties p, \un -> return (viewFacet un p lResult, viewFacet un p rResult)) ~~> trustedAnd @@ nobody
    outputVal <-
      (trustedAnd @@ nobody) `congruently` \un ->
        let ovs = un refl inputShares
         in xor (fst <$> ovs) && xor (snd <$> ovs)
    secretShare parties trustedAnd (alone, outputVal)
  XorGate l r -> do
    lResult <- compute l
    rResult <- compute r
    parties `parallel` \p un -> pure (viewFacet un p lResult /= viewFacet un p rResult)
  where
    compute = computeWire trustedAnd parties
    partyNames = toValue parties

mpc ::
  (Known [Party] parties, MonadIO m) =>
  Circuit parties ->
  Choreo ("trusted3rdParty" ': parties) (CLI m) ()
mpc circuit = do
  let parties = consSuper refl
  outputWire <- computeWire trusted3rdParty parties circuit
  result <- conclave parties $ reveal outputWire
  parties `parallel_` \p un -> putOutput "The resulting bit:" $ un p result


type Clients = '[
     "p1"
    ,"p2"
    --,"p3"
    --,"p4"
  ]

main :: IO ()
main = easyMain $ do parallel_ (allOf)  -- This step prevents problems with the order in which the clients come online.
                               (\p _ -> void $ getstr ("Press enter to indicate " ++ toValue p ++ " is ready:"))
                     forever $ mpc @Clients $ AndGate (InputWire p1) (InputWire p2)
