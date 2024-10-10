{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

{-
# Maximally fancy bookseller
-}

module BooksellerFancy where

import CLI
import Choreography
import Choreography.Network.Http
import Data (deliveryDateOf, priceOf)
import System.Environment

$(mkLoc "buyer")
$(mkLoc "seller")

proof1 :: Subset ("buyer" ': supporters) ("buyer" ': "seller" ': supporters)
proof1 = undefined

proof2 :: Subset '["buyer", "seller"] ("buyer" ': "seller" ': supporters)
proof2 = undefined

-- | `bookseller` is a choreography that implements the bookseller protocol.
-- This version takes a choreography `mkDecision` that implements the decision making process.
bookseller :: (KnownSymbols supporters) =>
  (Located '["buyer"] Int -> Choreo ("buyer" ': supporters) (CLI m) (Located '["buyer"] Bool)) ->
  Choreo ("buyer" ': "seller" ': supporters) (CLI m) ()
bookseller mkDecision = do
  database <- seller `_locally` getInput "Enter the book database (for `Read`):"
  title <- (buyer, getstr "Enter the title of the book to buy:") -~> seller @@ nobody

  -- the seller checks the price of the book and sends it to the buyer
  price <- (seller, \un -> return $ priceOf (un seller database) (un seller title)) ~~> buyer @@ nobody

  -- the buyer and supporters make a decision using the `mkDecision` choreography
  decision <- enclave proof1 $ mkDecision price

  -- get ride of the extra Located (TODO: make a helper function for this?)
  decision <- locally buyer $ \un -> return (un buyer (un buyer decision))

  -- if the buyer decides to buy the book, the seller sends the delivery date to the buyer
  enclave proof2 $ broadcast' First (First, decision) >>= \case
    True -> do
      deliveryDate <- (seller, \un -> return $ deliveryDateOf (un seller database) (un seller title)) ~~> buyer @@ nobody
      buyer `locally_` \un -> putstr "The book will be delivered on:" $ show (un buyer deliveryDate)
    False -> do
      buyer `_locally_` putNote "The book's price is out of the budget"

  return ()

-- | `mkDecision1` checks if buyer's budget is greater than the price of the book
mkDecision1 :: Located '["buyer"] Int -> Choreo '["buyer", supporter] (CLI m) (Located '["buyer"] Bool)
mkDecision1 price = do
  budget <- buyer `_locally` getInput "What are you willing to pay?"
  buyer `locally` \un -> return $ un buyer price <= un buyer budget

-- | `mkDecision2` asks buyer2 how much they're willing to contribute and checks
-- if the buyer's budget is greater than the price of the book minus buyer2's contribution
mkDecision2 :: Located '["buyer"] Int -> Choreo '["buyer", supporter] (CLI m) (Located '["buyer"] Bool)
mkDecision2 price = do
  budget <- buyer `_locally` getInput "What are you willing to pay?"
--   contrib2 <- (buyer2, getInput "How much you're willing to contribute?") -~> buyer @@ nobody
--   buyer `locally` \un -> return $ un buyer price - un buyer contrib2 <= un buyer contrib1a

  -- I don't know how to write this: basically I want to ask all the supporters how much they are willing to contributed and sum them up
  contrib <- undefined
  buyer `locally` \un -> return $ un buyer price <= un buyer budget

-- main :: IO ()
-- main = do
--   [loc] <- getArgs
--   _ <- case loc of
--     "buyer" -> runCLIIO $ runChoreography cfg choreo "buyer"
--     "seller" -> runCLIIO $ runChoreography cfg choreo "seller"
--     "buyer2" -> runCLIIO $ runChoreography cfg choreo "buyer2"
--     _ -> error "unknown party"
--   return ()
--   where
--     choreo = bookseller mkDecision2

--     cfg =
--       mkHttpConfig
--         [ ("buyer", ("localhost", 4242)),
--           ("seller", ("localhost", 4343)),
--           ("buyer2", ("localhost", 4444))
--         ]
