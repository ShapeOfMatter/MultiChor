{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Bookseller1Simple where

import CLI
import Choreography
import Choreography.Network.Http
import Data (deliveryDateOf, priceOf)
import System.Environment

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

  broadcast (buyer, decision) >>= \case
    True -> do
      deliveryDate <- seller `locally` \un -> return $ deliveryDateOf (un seller database) (un seller title')
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
  broadcast (buyer, inBuyerBudget) >>= \case
    True -> do
      deliveryDate <- (seller, \un -> return $ deliveryDateOf (un seller database) (un seller title)) ~~> buyer @@ nobody
      buyer `locally_` \un -> putOutput "The book will be delivered on:" $ un buyer deliveryDate
    False -> do
      buyer `_locally_` putNote "The book's price is out of the budget"

main :: IO ()
main = do
  [loc] <- getArgs
  delivery <- case loc of
    "buyer" -> runCLIIO $ runChoreography cfg bookseller' "buyer"
    "seller" -> runCLIIO $ runChoreography cfg bookseller' "seller"
    _ -> error "unknown party"
  print delivery
  where
    cfg =
      mkHttpConfig
        [ ("buyer", ("localhost", 4242)),
          ("seller", ("localhost", 4343))
        ]
