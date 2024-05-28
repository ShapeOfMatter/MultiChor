{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE TemplateHaskell #-}

{-
# Simple bookseller

The bookseller protocol defines an interaction between two participants: a
seller and a (would-be) buyer.  The protocol begins with the buyer sending the
title of a book they want to buy to the seller.  The seller replies with the
book’s price, and the buyer checks if the price is within their budget.  If the
buyer can afford the book, they inform the seller and get back a delivery date;
otherwise, they tell the seller they will not buy the book.

In this example, we assume the only books that a buyer can buy are `Types and
Programming Languages` and `Homotopy Type Theory` as `priceOf` and `deliveryOf`
are partial functions:

```haskell
priceOf :: String -> Int
priceOf "Types and Programming Languages" = 80
priceOf "Homotopy Type Theory"            = 120

deliveryDateOf :: String -> Day
deliveryDateOf "Types and Programming Languages" = fromGregorian 2022 12 19
deliveryDateOf "Homotopy Type Theory"            = fromGregorian 2023 01 01
```

It's straightforward to extend these definitions and make them total.

## Running the example

```bash
# in shell 1
cabal run bookseller-1-simple buyer

# in shell 2
cabal run bookseller-1-simple seller

# shell 1 will prompt the user to type in the book they want to buy
> Enter the title of the book to buy
Types and Programming Languages

# shell 1 will return the delivery date it receives from the seller or out of budget
# then both programs terminate
> The book will be delivered on 2022-12-19


# if we run the choreography again with a different book to buy

# in shell 1
cabal run bookseller-1-simple buyer

# in shell 2
cabal run bookseller-1-simple seller

# in shell 1
> Enter the title of the book to buy
Homotopy Type Theory

# in shell 1
> The book's price is out of the budget
```
-}

module Bookseller1Simple where

import Logic.Propositional (introAnd)

import Choreography
import Choreography.Network.Http
import System.Environment

import Data (deliveryDateOf, priceOf)
import CLI

$(mkLoc "buyer")
$(mkLoc "seller")
type Participants = ["buyer", "seller"]

-- | `bookseller` is a choreography that implements the bookseller protocol.
bookseller :: Choreo Participants (CLI m) ()
bookseller = do
  database <- seller `_locally` getInput "Enter the book database (for `Read`):"
  buyer_budget <- buyer `_locally` getInput "Enter your total budget:"
  title <- buyer `_locally` getstr "Enter the title of the book to buy:"

  title' <- (buyer, title) ~> seller @@ nobody
  price <- seller `locally` \un -> return $ priceOf (un seller database) (un seller title')
  price' <- (seller, price) ~> buyer @@ nobody
  decision <- buyer `locally` \un -> return $ un buyer price' <= un buyer buyer_budget

  broadcastCond (buyer `introAnd` buyer, decision) \case
    True  -> do
      deliveryDate  <- seller `locally` \un -> return $ deliveryDateOf (un seller database) (un seller title')
      deliveryDate' <- (seller, deliveryDate) ~> buyer @@ nobody
      buyer `locally_` \un -> putOutput "The book will be delivered on:" $ un buyer deliveryDate'
    False -> do
      buyer `_locally_` putNote "The book's price is out of the budget"

-- `bookseller'` is a simplified version of `bookseller` that utilizes `~~>`
bookseller' :: Choreo Participants (CLI m) ()
bookseller' = do
  database <- seller `_locally` getInput "Enter the book database (for `Read`):"
  buyer_budget <- buyer `_locally` getInput "Enter your total budget:"
  title <- (buyer, getstr "Enter the title of the book to buy:") -~> seller @@ nobody
  price <- (seller, \un -> return $ priceOf (un seller database) (un seller title)) ~~> buyer @@ nobody

  inBuyerBudget <- buyer `locally` (\un -> return $ un buyer price <= un buyer buyer_budget)
  broadcastCond (explicitMember `introAnd` buyer, inBuyerBudget) \case
    True  -> do
      deliveryDate <- (seller, \un -> return $ deliveryDateOf (un seller database) (un seller title)) ~~> buyer @@ nobody
      buyer `locally_` \un -> putOutput "The book will be delivered on:" $ un buyer deliveryDate
    False -> do
      buyer `_locally_` putNote "The book's price is out of the budget"


main :: IO ()
main = do
  [loc] <- getArgs
  delivery <- case loc of
    "buyer"  -> runCLIIO $ runChoreography cfg bookseller' "buyer"
    "seller" -> runCLIIO $ runChoreography cfg bookseller' "seller"
    _ -> error "unknown party"
  print delivery
  where
    cfg = mkHttpConfig [ ("buyer",  ("localhost", 4242))
                       , ("seller", ("localhost", 4343))
                       ]
