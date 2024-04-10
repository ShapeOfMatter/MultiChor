{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import Choreography (runChoreography)
import Choreography.Choreo
import Choreography.Location
import Choreography.Network.Http
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Proxy
import System.Environment
import Text.Read (readMaybe)

client :: Proxy "client"
client = Proxy

coordinator :: Proxy "coordinator"
coordinator = Proxy

alice :: Proxy "alice"
alice = Proxy

bob :: Proxy "bob"
bob = Proxy

type Participants = ["client", "coordinator", "alice", "bob"]

type State = (Int @ "alice", Int @ "bob")

type Action = (String, Int)

type Transaction = [Action]

-- | `validate` checks if a transaction can be executed while keeping balance >= 0
-- returns if the transaction satisfies the property and the balance after the transaction
validate :: String -> Int -> Transaction -> (Bool, Int)
validate name balance tx = foldl (\(valid, i) (_, amount) -> (let next = i + amount in (valid && next >= 0, next))) (True, balance) actions
  where
    actions = filter (\(n, _) -> n == name) tx

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
handleTransaction :: State -> Transaction @ "coordinator" -> Choreo Participants IO (Bool @ "coordinator", State)
handleTransaction (aliceBalance, bobBalance) tx = do
  -- Voting Phase
  txa <- (coordinator, tx) ~> alice
  voteAlice <- (alice, \unwrap -> do { return $ fst $ validate "alice" (unwrap aliceBalance) (unwrap txa) }) ~~> coordinator
  txb <- (coordinator, tx) ~> bob
  voteBob <- (bob, \unwrap -> do { return $ fst $ validate "bob" (unwrap bobBalance) (unwrap txb) }) ~~> coordinator

  -- Check if the transaction can be committed
  canCommit <- coordinator `locally` \unwrap -> do return $ unwrap voteAlice && unwrap voteBob

  -- Commit Phase
  cond (coordinator, canCommit) \case
    True -> do
      aliceBalance' <- alice `locally` \unwrap -> do return $ snd $ validate "alice" (unwrap aliceBalance) (unwrap txa)
      bobBalance' <- bob `locally` \unwrap -> do return $ snd $ validate "bob" (unwrap bobBalance) (unwrap txb)
      return (canCommit, (aliceBalance', bobBalance'))
    False -> do
      return (canCommit, (aliceBalance, bobBalance))

-- | `bank` loops forever and handles transactions.
bank :: State -> Choreo Participants IO ()
bank state = do
  client `locally` \_ -> do
    putStrLn "Command? (alice|bob {amount};)+"
  tx <- (client, \_ -> do { parse <$> getLine }) ~~> coordinator
  (committed, state') <- handleTransaction state tx
  committed' <- (coordinator, committed) ~> client
  client `locally` \unwrap -> do
    putStrLn if unwrap committed' then "Committed" else "Not committed"
  alice `locally` \unwrap -> do putStrLn ("Alice's balance: " ++ show (unwrap (fst state')))
  bob `locally` \unwrap -> do putStrLn ("Bob's balance: " ++ show (unwrap (snd state')))
  bank state' -- repeat
  return ()

-- | `startBank` is a choreography that initializes the states and starts the bank application.
startBank :: Choreo Participants IO ()
startBank = do
  aliceBalance <- alice `locally` \_ -> do return 0
  bobBalance <- bob `locally` \_ -> do return 0
  bank (aliceBalance, bobBalance)

main :: IO ()
main = do
  runChoreo startBank
