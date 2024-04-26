{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module DelegationFig20 where

{-
let choice : ()+()@[alice, bob] = com[alice][alice, bob] alices_choice;
let query : Query@[alice] = case[alice, bob] choice of
Inl _ => com[bob][alice] bobs_query;
Inr _ => alices_query;
let answerer : (Query@[carroll] -> Response@[carroll])@[carroll] = carrolls_func;
let response = com[carroll][bob, alice] (answerer (com[alice][carroll] query));
case[alice, bob] choice of
Inl _ => bobs_terminal response;
Inr _ => alices_terminal response;
 -}

import Choreography
import CLI
import Data (TestArgs, reference)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Logic.Propositional (introAnd)
import Logic.Classes (refl)
import Test.QuickCheck (Arbitrary, arbitrary, arbitraryPrintableChar, elements, listOf1)

$(mkLoc "alice")
$(mkLoc "bob")
$(mkLoc "carroll")
type Participants = ["alice", "bob", "carroll"]


data Args = Args{ choice :: Bool
                , aliceQ :: String
                , bobQ :: String
                , carrollF :: String
                } deriving (Eq, Show, Read)
data Result = Result{ aliceR :: [String]
                    , bobR :: [String]
                    , carrollR :: [String]
                    } deriving (Eq, Show, Read)
instance TestArgs Args Result where
  reference Args{choice, aliceQ, bobQ, carrollF} =
    let f = fromMaybe carrollsDefault $ carrollF `lookup` carrollsFunctions
        result = f $ if choice then aliceQ else bobQ
    in Result{aliceR=[result | choice], bobR=[result | not choice], carrollR=[]}
instance Arbitrary Args where
  arbitrary = Args <$> arbitrary
                   <*> listOf1 arbitraryPrintableChar  -- Avoiding wierd behavior of CLI on empty outputs :(
                   <*> listOf1 arbitraryPrintableChar
                   <*> elements (fst <$> carrollsFunctions)

carrollsFunctions :: [(String, String -> String)]
carrollsFunctions = [ ("reverse", reverse)
                     , ("alphabetize", sort)
                     ]
carrollsDefault :: String -> String
carrollsDefault = const "No Handler"

mainCho :: Choreo Participants (CLI m) ()
mainCho = do
  choice <- (alice, \_ -> getInput "Alice's choice:") ~~> (alice @@ (bob @@ nobody))
  query <- flatten ((alice @@ nobody) `introAnd` (alice @@ nobody) `introAnd` (alice @@ nobody)) =<<
    cond (refl `introAnd` explicitSubset, choice) \case
      False -> (bob, \_ -> getstr "Bob's query:") ~~> (alice @@ nobody)
      True  -> alice `_locally` getstr "Alice's query:"
  answerer <- carroll `_locally` do handlerName <- getstr "Carrol's function (reverse or alphabetize):"
                                    return $ fromMaybe carrollsDefault $ handlerName `lookup` carrollsFunctions
  query' <- (alice `introAnd` alice, query) ~> (carroll @@ nobody)
  response <- (carroll, \un -> return $ un carroll answerer (un carroll query')) ~~> (alice @@ (bob @@ nobody))
  (_ :: Located '["alice", "bob"] ()) <- cond (refl `introAnd` explicitSubset, choice) \case
    False -> bob `locally_` \un -> putstr "Recieved:" (un bob response)
    True -> alice `locally_` \un -> putstr "Recieved:" (un alice response)
  return ()

