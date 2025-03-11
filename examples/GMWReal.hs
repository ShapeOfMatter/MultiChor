{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module GMWReal where

import CLI
import Choreography
import Choreography.Network.Http
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Random.Types qualified as CRT
import Data (TestArgs, reference)
import Data.Foldable (toList)
import Data.Kind (Type)
import Data.Maybe (fromJust)
import GHC.TypeLits (KnownSymbol)
import ObliviousTransfer
import System.Environment
import System.Random
import Test.QuickCheck (Arbitrary, arbitrary, chooseInt, elements, getSize, oneof, resize)

$(mkLoc "trusted3rdParty")
$(mkLoc "p1")
$(mkLoc "p2")
$(mkLoc "p3")
$(mkLoc "p4")

xor :: (Foldable f) => f Bool -> Bool
xor = foldr1 (/=)

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
  arbitrary = do
    size <- getSize
    if 1 >= size
      then oneof $ (LitWire <$> arbitrary) : (pure <$> [InputWire p1, InputWire p2, InputWire p3, InputWire p4])
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
        InputWire p -> fromJust $ toLocTm p `lookup` inputs
        LitWire b -> b
        AndGate left right -> recurse left && recurse right
        XorGate left right -> recurse left /= recurse right
      inputs = ["p1", "p2", "p3", "p4"] `zip` [p1in, p2in, p3in, p4in]
      answer = recurse circuit

genShares :: forall ps p m. (MonadIO m, KnownSymbols ps) => Member p ps -> Bool -> m (Quire ps Bool)
genShares p x = quorum1 p gs'
  where
    gs' :: forall q qs. (KnownSymbol q, KnownSymbols qs) => m (Quire (q ': qs) Bool)
    gs' = do
      freeShares <- sequence $ pure $ liftIO randomIO -- generate n-1 random shares
      return $ qCons (xor (qCons @q x freeShares)) freeShares

secretShare ::
  forall parties p m.
  (KnownSymbols parties, KnownSymbol p, MonadIO m) =>
  Member p parties ->
  Located '[p] Bool ->
  Choreo parties m (Faceted parties '[] Bool)
secretShare p value = do
  shares <- locally p \un -> genShares p (un singleton value)
  PIndexed fs <- scatter p (allOf @parties) shares
  return $ PIndexed $ Facet . othersForget (First @@ nobody) . getFacet . fs

reveal :: forall ps m. (KnownSymbols ps) => Faceted ps '[] Bool -> Choreo ps m Bool
reveal shares = xor <$> (gather ps ps shares >>= naked ps)
  where
    ps = allOf @ps

-- use OT to do multiplication
fAnd ::
  forall parties m.
  (KnownSymbols parties, MonadIO m, CRT.MonadRandom m) =>
  Faceted parties '[] Bool ->
  Faceted parties '[] Bool ->
  Choreo parties (CLI m) (Faceted parties '[] Bool)
fAnd uShares vShares = do
  let genBools = sequence $ pure randomIO
  a_j_s :: Faceted parties '[] (Quire parties Bool) <- _parallel (allOf @parties) genBools
  bs :: Faceted parties '[] Bool <- fanOut \p_j -> do
    let p_j_name = toLocTm p_j
    b_i_s <- fanIn (p_j @@ nobody) \p_i ->
      if toLocTm p_i == p_j_name
        then _locally p_j $ pure False
        else do
          -- bb is the truth table
          bb <- locally p_i \un ->
            let a_ij = getLeaf (viewFacet un p_i a_j_s) p_j
                u_i = viewFacet un p_i uShares
             in pure (xor [u_i, a_ij], a_ij)
          -- localize p_j vSHares is party j's share of v
          conclaveTo (p_i @@ p_j @@ nobody) (listedSecond @@ nobody) (ot2 bb $ localize p_j vShares)
    locally p_j \un -> pure $ xor $ un singleton b_i_s
  parallel (allOf @parties) \p_i un ->
    let computeShare u v a_js b = xor $ [u && v, b] ++ toList (qModify p_i (const False) a_js)
     in pure $
          computeShare
            (viewFacet un p_i uShares)
            (viewFacet un p_i vShares)
            (viewFacet un p_i a_j_s)
            (viewFacet un p_i bs)

gmw ::
  forall parties m.
  (KnownSymbols parties, MonadIO m, CRT.MonadRandom m) =>
  Circuit parties ->
  Choreo parties (CLI m) (Faceted parties '[] Bool)
gmw circuit = case circuit of
  InputWire p -> do
    -- process a secret input value from party p
    value :: Located '[p] Bool <- _locally p $ getInput "Enter a secret input value:"
    secretShare p value
  LitWire b -> do
    -- process a publicly-known literal value
    let chooseShare :: forall p. (KnownSymbol p) => Member p parties -> Choreo parties (CLI m) (Located '[p] Bool)
        chooseShare p = congruently (p @@ nobody) $ \_ -> case p of
          First -> b
          Later _ -> False
    fanOut chooseShare
  AndGate l r -> do
    -- process an AND gate
    lResult <- gmw l
    rResult <- gmw r
    fAnd lResult rResult
  XorGate l r -> do
    -- process an XOR gate
    lResult <- gmw l
    rResult <- gmw r
    parallel (allOf @parties) \p un -> pure $ xor [viewFacet un p lResult, viewFacet un p rResult]

mpc ::
  forall parties m.
  (KnownSymbols parties, MonadIO m, CRT.MonadRandom m) =>
  Circuit parties ->
  Choreo parties (CLI m) ()
mpc circuit = do
  outputWire <- gmw circuit
  result <- reveal outputWire
  void $ _parallel (allOf @parties) $ putOutput "The resulting bit:" result

mpcmany ::
  (KnownSymbols parties, MonadIO m, CRT.MonadRandom m) =>
  Circuit parties ->
  Choreo parties (CLI m) ()
mpcmany circuit = do
  mpc circuit

type Clients = '["p1", "p2"] -- , "p3", "p4"]

main :: IO ()
main = do
  let circuit :: Circuit Clients = AndGate (LitWire True) (LitWire True)
  [loc] <- getArgs
  delivery <- case loc of
    "p1" -> runCLIIO $ runChoreography cfg (mpcmany @Clients circuit) "p1"
    "p2" -> runCLIIO $ runChoreography cfg (mpcmany @Clients circuit) "p2"
    --    "p3" -> runCLIIO $ runChoreography cfg (mpcmany @Clients circuit) "p3"
    --    "p4" -> runCLIIO $ runChoreography cfg (mpcmany @Clients circuit) "p4"
    _ -> error "unknown party"
  print delivery
  where
    cfg =
      mkHttpConfig
        [ ("p1", ("localhost", 4242)),
          ("p2", ("localhost", 4343))
          --                       , ("p3", ("localhost", 4344))
          --                       , ("p4", ("localhost", 4345))
        ]
