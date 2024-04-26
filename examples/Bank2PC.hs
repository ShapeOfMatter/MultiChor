{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-
# Example: bank-2pc

## Overview

This example implements the two-phase commit protocol.

This system models a bank. Alice and Bob each have their accounts on the bank, and the balance of each account is stored at separate locations `alice` and `bob`. This bank has a policy that does not allow balances to be negative.

As a bank administrator, you can change the balance of accounts. An `Action` changes a single account's balance by specifying the account (`String`, "alice" or "bob") and the amount to add to the account (Int, can be negative to indicate withdrawal). A `Transaction` is a list of `Action`s.

When executing a `Transaction`, the system will perform Alice's actions at `alice` and Bob's actions at `bob`. However, the transaction aborts if one of the actions violates the bank policy and makes the balance negative. We use the two-phase commit protocol to ensure consistency between `alice` and `bob`.

In the voting phase, `alice` and `bob` check if the given transaction violates the policy. If one or more locations determine that the transaction violates the policy, the entire transaction aborts. If the transaction is valid, each location will commit the transaction.

## Execution

For simplicity, this example uses `runChoreo` and executes the choreography directly. On the terminal, you can write transactions as follows. Both accounts' balances are initialized to 0.

```text
> cabal run bank-2pc
Command? (alice|bob {amount};)+
alice 10; bob 10                  # deposit 10 each to Alice and Bob's accounts
Committed
Alice's balance: 10
Bob's balance: 10
Command? (alice|bob {amount};)+   # deposit 20 to Alice's and withdraw 10 from Bob's
alice 20; bob -5
Committed
Alice's balance: 30
Bob's balance: 5
Command? (alice|bob {amount};)+   # move 10 from Bob to Alice (Invalid, Bob's account will be negative)
alice 10; bob -10
Not committed
Alice's balance: 30
Bob's balance: 5
```
-}

module Bank2PC where

import Choreography
import CLI
import Control.Monad (unless)
import Data (TestArgs, reference)
import Data.List (intercalate, transpose)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy(Proxy))
import GHC.TypeLits (KnownSymbol, symbolVal)
import Logic.Propositional (introAnd)
import Test.QuickCheck (Arbitrary, arbitrary, elements, listOf, listOf1)
import Text.Read (readMaybe)

$(mkLoc "client")
$(mkLoc "coordinator")
$(mkLoc "alice")
$(mkLoc "bob")
type Participants = ["client", "coordinator", "alice", "bob"]

type State = (Located '["alice"] Int, Located '["bob"] Int)

type Action = (String, Int)

type Transaction = [Action]

newtype Args p q = Args [Transaction] deriving (Eq, Show, Read)
instance (KnownSymbol p, KnownSymbol q) => TestArgs (Args p q) [[String]] where
 reference (Args tx) = addCoordinator . transpose $ showAll <$> ref start tx
   where start = (, 0) <$> [symbolVal (Proxy @p), symbolVal (Proxy @q)]
         ref _ [] = []
         ref state (t:ts) = let (s', r) = refAs state t
                                s'' = if r then s' else state
                            in (r, s'') : ref s'' ts
         refAs state [] = (state, True)
         refAs state (a:as) = let (s', r) = refA state a
                              in if r then refAs s' as else (state, False)
         refA state (name, amount) = let (otherL, (_, s):otherR) = ((== name) . fst) `break` state
                                         s' = s + amount
                                     in (otherL ++ ((name, s'):otherR), 0 <= s')
         showAll :: (Bool, [(String, Int)]) -> [String]
         showAll (clnt, servers) = show clnt : (show . snd <$> servers)
         addCoordinator (clnt:servers) = clnt : [] : servers
         addCoordinator _ = error "this can't happen, right? I could enforce it by types, but it's a core..."
instance (KnownSymbol p, KnownSymbol q) => Arbitrary (Args p q) where
  arbitrary = (Args . (++ [[]]) <$>) . listOf . listOf1 $ (,) <$> elements [symbolVal $ Proxy @p, symbolVal $ Proxy @q] <*> arbitrary

-- | `validate` checks if a transaction can be executed while keeping balance >= 0
-- returns if the transaction satisfies the property and the balance after the transaction
validate :: String -> Int -> Transaction -> (Bool, Int)
validate name balance tx = foldl (\(valid, i) (_, amount) -> (let next = i + amount in (valid && next >= 0, next))) (True, balance) actions
  where
    actions = filter (\(n, _) -> n == name) tx

render :: Transaction -> String
render txns = intercalate ";" $ (\(a,b) -> a ++ " " ++ show b) <$> txns

-- | `parse` converts the user input into a transaction
parse :: String -> Transaction
parse s = tx
  where
    t = splitOn ";" s
    f :: String -> Maybe Action
    f l = do
      [target, amountStr] <- return $ words l
      amount <- readMaybe amountStr :: Maybe Int
      target' <- if target == "alice" || target == "bob" then Just target else Nothing
      return (target', amount)
    tx = mapMaybe f t

-- | `handleTransaction` is a choreography that handles a transaction.
-- Given the current state and a transaction, it will first ask alice and bob to vote,
-- then it will decide whether to commit the transaction or not.
-- If the transaction is committed, it will update the state.
-- Otherwise, it will keep the state unchanged.
handleTransaction :: (Monad m) =>
                     State ->
                     Located '["coordinator"] Transaction  ->
                     Choreo Participants m (Located '["coordinator"] Bool, State)
handleTransaction (aliceBalance, bobBalance) tx = do
  -- Voting Phase
  txa <- (coordinator `introAnd` coordinator, tx) ~> (alice @@ nobody)
  voteAlice <- (alice, \un -> do { return $ fst $ validate "alice" (un alice aliceBalance) (un alice txa) }) ~~> (coordinator @@ nobody)
  txb <- (coordinator `introAnd` coordinator, tx) ~> (bob @@ nobody)
  voteBob <- (bob, \un -> do { return $ fst $ validate "bob" (un bob bobBalance) (un bob txb) }) ~~> (coordinator @@ nobody)

  -- Check if the transaction can be committed
  canCommit <- coordinator `locally` \un -> do return $ un coordinator voteAlice && un coordinator voteBob

  -- Commit Phase
  broadcastCond (coordinator `introAnd` coordinator, canCommit) \case
    True -> do
      aliceBalance' <- alice `locally` \un -> do return $ snd $ validate "alice" (un alice aliceBalance) (un alice txa)
      bobBalance' <- bob `locally` \un -> do return $ snd $ validate "bob" (un bob bobBalance) (un bob txb)
      return (canCommit, (aliceBalance', bobBalance'))
    False -> do
      return (canCommit, (aliceBalance, bobBalance))

-- | `bank` loops forever and handles transactions.
bank :: State -> Choreo Participants (CLI m) ()
bank state = do
  tx <- (client, \_ -> parse <$> getstr "Command? (alice|bob {amount};)+"
        ) ~~> (coordinator @@ nobody)
  (committed, state') <- handleTransaction state tx
  committed' <- (coordinator `introAnd` coordinator, committed) ~> (client @@ nobody)
  client `locally_` \un -> putOutput "Committed?" (un client committed')
  alice `locally_` \un -> putOutput "Alice's balance:" (un alice (fst state'))
  bob `locally_` \un -> putOutput "Bob's balance:" (un bob (snd state'))
  cond' (coordinator, \un -> return $ null $ un coordinator tx) (`unless` bank state') -- repeat

-- | `startBank` is a choreography that initializes the states and starts the bank application.
startBank :: Choreo Participants (CLI m) ()
startBank = do
  aliceBalance <- alice `_locally` return 0
  bobBalance <- bob `_locally` return 0
  bank (aliceBalance, bobBalance)

main :: IO ()
main = do
  runCLIIO $ runChoreo startBank
