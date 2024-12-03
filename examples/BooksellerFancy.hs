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

-- | `bookseller` is a choreography that implements the bookseller protocol.
-- This version takes a choreography `mkDecision` that implements the decision making process.
bookseller :: forall supporters {m}. (KnownSymbols supporters) =>
  (Located '["buyer"] Int -> Choreo ("buyer" ': supporters) (CLI m) (Located '["buyer"] Bool)) ->
  Choreo ("buyer" ': "seller" ': supporters) (CLI m) ()
bookseller mkDecision = do
  database <- seller `_locally` getInput "Enter the book database (for `Read`):"
  title <- (buyer, getstr "Enter the title of the book to buy:") -~> seller @@ nobody

  -- the seller checks the price of the book and sends it to the buyer
  price <- (seller, \un -> return $ priceOf (un seller database) (un seller title)) ~~> buyer @@ nobody

  -- the buyer and supporters (transactors) make a decision using the `mkDecision` choreography
  decision <- enclave transactors $ mkDecision price

  -- if the buyer decides to buy the book, the seller sends the delivery date to the buyer
  _ <- enclave buyerAndSeller $
    broadcast (buyer, flatten explicitSubset allOf decision) >>= \case
      True -> do
        deliveryDate <- (seller, \un -> return $ deliveryDateOf (un seller database) (un seller title)) ~~> buyer @@ nobody
        buyer `locally_` \un -> putstr "The book will be delivered on:" $ show (un buyer deliveryDate)
      False -> do
        buyer `_locally_` putNote "The book's price is out of the budget"

  return ()
  where
    transactors :: Subset ("buyer" ': supporters) ("buyer" ': "seller" ': supporters)
    transactors = explicitMember @@ (consSuper . consSuper $ refl)

    buyerAndSeller :: Subset '["buyer", "seller"] ("buyer" ': "seller" ': supporters)
    buyerAndSeller = explicitSubset

-- | `mkDecision1` checks if buyer's budget is greater than the price of the book
mkDecision1 :: Located '["buyer"] Int -> Choreo ("buyer" ': supporters) (CLI m) (Located '["buyer"] Bool)
mkDecision1 price = do
  budget <- buyer `_locally` getInput "What are you willing to pay?"
  buyer `locally` \un -> return $ un buyer price <= un buyer budget

-- | `mkDecision2` asks supporters how much they're willing to contribute and checks
-- if the buyer's budget is greater than the price of the book minus all supporters' contribution
mkDecision2 :: (KnownSymbols supporters) => Located '["buyer"] Int -> Choreo ("buyer" ': supporters) (CLI m) (Located '["buyer"] Bool)
mkDecision2 price = do
  budget <- buyer `_locally` getInput "What are you willing to pay?"
  --   contrib2 <- (buyer2, getInput "How much you're willing to contribute?") -~> buyer @@ nobody
  --   buyer `locally` \un -> return $ un buyer price - un buyer contrib2 <= un buyer contrib1a

  contribs <- fanIn explicitSubset $ \supporter ->
    (supporter, getInput "How much you're willing to contribute?") -~> buyer @@ nobody
  contrib <-
    buyer `locally` \un ->
      return $ sum (un buyer contribs)
  buyer `locally` \un -> return $ un buyer price <= un buyer budget - un buyer contrib

main :: IO ()
main = do
  [loc] <- getArgs
  _ <- case loc of
    "buyer" -> runCLIIO $ runChoreography cfg choreo "buyer"
    "seller" -> runCLIIO $ runChoreography cfg choreo "seller"
    "buyer2" -> runCLIIO $ runChoreography cfg choreo "buyer2"
    "buyer3" -> runCLIIO $ runChoreography cfg choreo "buyer3"
    _ -> error "unknown party"
  return ()
  where
    choreo = bookseller @["buyer2", "buyer3"] mkDecision2

    cfg =
      mkHttpConfig
        [ ("buyer", ("localhost", 4242)),
          ("seller", ("localhost", 4343)),
          ("buyer2", ("localhost", 4444)),
          ("buyer3", ("localhost", 45454))
        ]
