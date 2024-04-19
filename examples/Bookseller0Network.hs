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

import Choreography.Network
import Choreography.Network.Http
import Data.Time
import System.Environment

import Data (Database, defaultBudget, deliveryDateOf, priceOf, textbooks)

buyer :: Int -> String -> Network IO (Maybe Day)
buyer budget title = do
  send title ["seller"]
  price <- recv "seller"
  if price < budget
  then do
    send True ["seller"]
    (deliveryDate :: Day) <- recv "seller"
    return $ Just deliveryDate
  else do
    send False ["seller"]
    return Nothing

seller :: Database -> Network IO ()
seller books = do
  title <- recv "buyer"
  send (priceOf books title) ["buyer"]
  decision <- recv "buyer"
  if decision
  then do
    send (deliveryDateOf books title) ["buyer"]
  else do
    return ()

main :: IO ()
main = do
  [loc] <- getArgs
  case loc of
    "buyer" -> do putStrLn "Enter the title of the book to buy:"
                  title <- getLine
                  result <- runNetwork cfg "buyer" $ buyer defaultBudget title
                  case result of
                    Nothing -> putStrLn "The book's price is out of the budget"
                    Just day -> putStrLn ("The book will be delivered on " ++ show day)
    "seller" -> runNetwork cfg "seller" $ seller textbooks
    _ -> error "unknown party"
  return ()
  where
    cfg = mkHttpConfig [ ("buyer",  ("localhost", 4242))
                       , ("seller", ("localhost", 4343))
                       ]
