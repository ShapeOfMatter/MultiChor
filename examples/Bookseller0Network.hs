module Bookseller0Network where

{-
# Bookseller as individual network programs

This example implmenets the bookseller protocol as individual network
programs (non-choreographic way). See
[`bookseller-1-simple`](../bookseller-1-simple) for a description of
the protocol.

## Running the example

Same as [`bookseller-1-simple`](../bookseller-1-simple) but with `cabal run bookseller-0-network`.
-}

import CLI
import Choreography.Network
import Choreography.Network.Http
import Data (deliveryDateOf, priceOf)
import Data.Time
import System.Environment

buyer :: Network (CLI m) ()
buyer = do
  budget <- run $ getInput @Int "Enter your total budget:"
  title <- run $ getstr "Enter the title of the book to buy:"
  send title ["seller"]
  price <- recv "seller"
  if price <= budget
    then do
      send True ["seller"]
      (deliveryDate :: Day) <- recv "seller"
      run $ putOutput "The book will be delivered on:" deliveryDate
    else do
      send False ["seller"]
      run $ putNote "The book's price is out of the budget"

seller :: Network (CLI m) ()
seller = do
  database <- run $ getInput "Enter the book database (for `Read`):"
  title <- recv "buyer"
  send (database `priceOf` title) ["buyer"]
  decision <- recv "buyer"
  if decision
    then do
      send (database `deliveryDateOf` title) ["buyer"]
    else do
      return ()

main :: IO ()
main = do
  [loc] <- getArgs
  case loc of
    "buyer" -> runCLIIO $ runNetwork cfg "buyer" buyer
    "seller" -> runCLIIO $ runNetwork cfg "seller" seller
    _ -> error "unknown party"
  return ()
  where
    cfg =
      mkHttpConfig
        [ ("buyer", ("localhost", 4242)),
          ("seller", ("localhost", 4343))
        ]
