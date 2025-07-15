{-# LANGUAGE FunctionalDependencies #-}

module Data where

import Data.Time (Day (ModifiedJulianDay), fromGregorian)
import Test.QuickCheck (Arbitrary, Positive (Positive), arbitrary, arbitraryPrintableChar, elements, getPositive, listOf1)

data Book = Book
  { name :: String,
    price :: Int,
    deliverable :: Day
  }
  deriving (Eq, Read, Show)

instance Arbitrary Book where
  arbitrary =
    Book
      <$> listOf1 arbitraryPrintableChar
      <*> (getPositive <$> arbitrary)
      <*> (ModifiedJulianDay . getPositive <$> arbitrary)

type Database = [Book]

textbooks :: Database
textbooks =
  [ Book
      { name = "Types and Programming Languages",
        price = 80,
        deliverable = fromGregorian 2023 12 19
      },
    Book
      { name = "Homotopy Type Theory",
        price = 120,
        deliverable = fromGregorian 2023 09 18
      }
  ]

unsafeQuietHead :: [a] -> a
unsafeQuietHead [] = error "Someone called head on an empty list"
unsafeQuietHead (x:_) = x
unsafeQuietTail :: [a] -> [a]
unsafeQuietTail [] = error "Someone called tail on an empty list"
unsafeQuietTail (_:xs) = xs

priceOf :: Database -> String -> Int
priceOf books title = price $ unsafeQuietHead $ filter ((== title) . name) books

deliveryDateOf :: Database -> String -> Day
deliveryDateOf books title = deliverable $ unsafeQuietHead $ filter ((== title) . name) books

data BooksellerArgs = BooksellerArgs
  { books :: Database,
    choice :: String,
    budget :: Int
  }
  deriving (Read, Show)

instance Arbitrary BooksellerArgs where
  arbitrary = do
    books <- listOf1 arbitrary
    choice <- name <$> elements books
    budget <- getPositive <$> arbitrary
    return BooksellerArgs {books, choice, budget}

class TestArgs a r | a -> r where
  reference :: a -> r

instance TestArgs BooksellerArgs (Maybe Day) where
  reference (BooksellerArgs {books, choice, budget}) =
    if budget < priceOf books choice
      then Nothing
      else Just $ deliveryDateOf books choice

instance TestArgs (BooksellerArgs, Positive Int) (Maybe Day) where
  reference (BooksellerArgs {books, choice, budget}, Positive contrib) =
    if budget < (priceOf books choice - contrib)
      then Nothing
      else Just $ deliveryDateOf books choice

-- a scenario where there's four buyers (one primary and three supporter)
instance TestArgs (BooksellerArgs, Positive Int, Positive Int, Positive Int) (Maybe Day) where
  reference (BooksellerArgs {books, choice, budget}, Positive contrib, Positive contrib2, Positive contrib3) =
    if budget < (priceOf books choice - contrib - contrib2 - contrib3)
      then Nothing
      else Just $ deliveryDateOf books choice
