{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE TemplateHaskell #-}

{-
# Location-polymorphic bookseller

This example implements the location-polymorphic bookseller where the
buyer's location is abstracted away as an argument.

For a general description of the protocol, see [`bookseller-1-simple`](../bookseller-1-simple).

# Running the protocol

Same as [`bookseller-1-simple`](../bookseller-1-simple) but with `cabal run bookseller-3-loc-poly`.
-}

module Bookseller3LocPoly where

import Choreography
import Choreography.Network.Http
import CLI
import Data (deliveryDateOf, priceOf)
import GHC.TypeLits
import System.Environment
import GDP (introAnd)

$(mkLoc "buyer")
$(mkLoc "seller")
--type Participants = ["buyer", "seller"]

-- | `bookseller` is a choreography that implements the bookseller protocol.
-- This version takes the name of the buyer as a parameter (`someBuyer`).
bookseller :: (KnownSymbol a, KnownSymbols ps) => Member a ps -> Choreo ("seller" ': ps) (CLI m) ()
bookseller someBuyer = do
  let theBuyer = inSuper consSet someBuyer
  database <- seller `_locally` getInput "Enter the book database (for `Read`):"
  buyer_budget <- theBuyer `_locally` getInput "Enter your total budget:"
  -- the buyer reads the title of the book and sends it to the seller
  title <- (theBuyer, getstr "Enter the title of the book to buy") -~> seller @@ nobody
  -- the seller checks the price of the book and sends it to the buyer
  price <- (seller, \un -> return $ priceOf (un seller database) (un seller title)) ~~> theBuyer @@ nobody

  inBuyerBudget <- theBuyer `locally` (\un -> return $ un singleton price <= un singleton buyer_budget)
  broadcastCond (explicitMember `introAnd` theBuyer, inBuyerBudget) \case
    True  -> do
      deliveryDate <- (seller, \un -> return $ deliveryDateOf (un seller database) (un seller title)) ~~> theBuyer @@ nobody

      theBuyer `locally_` \un -> putOutput "The book will be delivered on:" $ un singleton deliveryDate

    False -> do
      theBuyer `_locally_` putNote "The book's price is out of the budget"


main :: IO ()
main = do
  [loc] <- getArgs
  _ <- case loc of
    "buyer"  -> runCLIIO $ runChoreography cfg choreo "buyer"
    "seller" -> runCLIIO $ runChoreography cfg choreo "seller"
    _ -> error "unknown party"
  return ()
  where
    choreo = bookseller $ singleton @"buyer"

    cfg = mkHttpConfig [ ("buyer",  ("localhost", 4242))
                       , ("seller", ("localhost", 4343))
                       ]
