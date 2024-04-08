module Data where

import Data.Time
import System.Environment


defaultBudget :: Int
defaultBudget = 100

data Book = Book{ name :: String
                , price :: Int
                , deliverable :: Day}

type Database = [Book]

textbooks :: Database
textbooks = [Book{name = "Types and Programming Languages"
                 ,price = 80
                 ,deliverable = fromGregorian 2023 12 19}
            ,Book{name = "Homotopy Type Theory"
                 ,price = 120
                 ,deliverable = fromGregorian 2023 09 18}]

priceOf :: Database -> String -> Int
priceOf books title = price $ head $ filter ((== title) . name) books

deliveryDateOf :: Database -> String -> Day
deliveryDateOf books title = deliverable $ head $ filter ((== title) . name) books

