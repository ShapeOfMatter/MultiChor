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
import Data.Time
import System.Environment

import Data (defaultBudget, deliveryDateOf, priceOf, textbooks)

$(mkLoc "buyer")
$(mkLoc "seller")
type Participants = ["buyer", "seller"]

-- | `bookseller` is a choreography that implements the bookseller protocol.
bookseller :: String -> Choreo Participants IO (Maybe Day)
bookseller userTitle = do
  -- the buyer node prompts the user to enter the title of the book to buy
  title <- buyer `locally` \_ -> return userTitle
  -- the buyer sends the title to the seller
  title' <- (buyer `introAnd` buyer, title) ~> (seller @@ nobody)

  -- the seller checks the price of the book
  price <- seller `locally` \un -> return $ priceOf textbooks (un seller title')
  -- the seller sends back the price of the book to the buyer
  price' <- (seller `introAnd` seller, price) ~> (buyer @@ nobody)

  -- the buyer decides whether to buy the book or not
  decision <- buyer `locally` \un -> return $ un buyer price' < budget

  -- if the buyer decides to buy the book, the seller sends the delivery date to the buyer
  delivery <- cond (buyer `introAnd` buyer, decision) \case
    True  -> do
      deliveryDate  <- seller `locally` \un -> return $ deliveryDateOf textbooks (un seller title')
      deliveryDate' <- (seller `introAnd` seller, deliveryDate) ~> (buyer @@ nobody)

      buyer `locally` \un -> do
        putStrLn $ "The book will be delivered on " ++ show (un buyer deliveryDate')
        return $ Just (un buyer deliveryDate')

    False -> do
      buyer `locally` \_ -> do
        putStrLn "The book's price is out of the budget"
        return Nothing
  reveal (buyer `introAnd` buyer) delivery

-- `bookseller'` is a simplified version of `bookseller` that utilizes `~~>`
bookseller' :: String -> Choreo Participants IO (Maybe Day)
bookseller' userTitle = do
  title <- (buyer, \_ -> do
               return userTitle
           )
           ~~> (seller @@ nobody)

  price <- (seller, \un -> return $ priceOf textbooks (un seller title)) ~~> (buyer @@ nobody)

  delivery <- cond' (buyer, \un -> return $ un buyer price < budget) \case
    True  -> do
      deliveryDate <- (seller, \un -> return $ deliveryDateOf textbooks (un seller title)) ~~> (buyer @@ nobody)

      buyer `locally` \un -> do
        putStrLn $ "The book will be delivered on " ++ show (un buyer deliveryDate)
        return $ Just (un buyer deliveryDate)

    False -> do
      buyer `locally` \_ -> do
        putStrLn "The book's price is out of the budget"
        return Nothing
  reveal (buyer `introAnd` buyer) delivery

budget :: Int
budget = defaultBudget

main :: IO ()
main = do
  [loc] <- getArgs
  putStrLn "Enter the title of the book to buy"
  title <- getLine
  delivery <- case loc of
    "buyer"  -> runChoreography cfg (bookseller' title) "buyer"
    "seller" -> runChoreography cfg (bookseller' title) "seller"
    _ -> error "unknown party"
  print delivery
  where
    cfg = mkHttpConfig [ ("buyer",  ("localhost", 4242))
                       , ("seller", ("localhost", 4343))
                       ]