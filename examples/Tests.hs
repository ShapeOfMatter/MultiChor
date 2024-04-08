module Tests  where

import Control.Concurrent.Async (mapConcurrently)
import Data.Either (isRight)
import Distribution.TestSuite (Test)
import Distribution.TestSuite.QuickCheck
import Test.QuickCheck ( (===)
                       , Arbitrary (arbitrary)
                       , arbitraryBoundedIntegral
                       , counterexample
                       , elements
                       , Gen
                       , ioProperty
                       , Property
                       , Testable
                       , vectorOf)

import qualified Bookseller0Network
import Choreography.Network (runNetwork)
import Choreography.Network.Local (mkLocalConfig)
import Data (defaultBudget, deliverable, price, textbooks)
import qualified Data

tests :: IO [Test]
tests = return $ tests'

normalSettings :: TestArgs
normalSettings = stdTestArgs { verbosity = Verbose }

getNormalPT :: Testable prop => PropertyTest prop -> Test
getNormalPT = getPropertyTestWith normalSettings

tests' :: [Test]
tests' = [

  getNormalPT PropertyTest {
    name = "tautology",
    tags = [],
    property = \i -> (===) @Int i i
    },

  getNormalPT PropertyTest {
    name = "bookseller-0-network",
    tags =[],
    property = do book <- elements textbooks  -- The Gen Monad. Doing it this way kinda breaks failure-case-printing :(
                  let processes = [("seller", Bookseller0Network.seller textbooks >> return Nothing)
                                  ,("buyer", Bookseller0Network.buyer defaultBudget (Data.name book))]
                  return $ ioProperty $ do
                      config <- mkLocalConfig (fst <$> processes)  -- The IO Monad
                      [Nothing, delivery] <- mapConcurrently
                                             (\(name, process) -> runNetwork config name process)
                                             processes
                      case delivery of
                        Nothing -> return $ defaultBudget < (price book)
                        Just d -> return $ defaultBudget >= (price book) && d == (deliverable book)
  }
  ]

{-testEasyCompute :: Test
testEasyCompute = testProperty "Easy Compute" $ case compute of
    Compute (Location os' _, Variable "a") (_, Literal (_, Bit True)) | os' == os && os == top -> True
    _ -> error (pretty compute)
  where compute :: Statement Located
        Right [(Location os _, compute)] = validate mempty program
        Right program = runParser programParser () "hardcoded example" "a = 1"
-}
