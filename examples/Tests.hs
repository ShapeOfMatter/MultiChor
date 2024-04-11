module Tests  where

import Control.Concurrent.Async (mapConcurrently)
import Data.Either (isRight)
import Data.List (nub)
import Distribution.TestSuite (Test)
import Distribution.TestSuite.QuickCheck
import Test.QuickCheck ( (===)
                       , (.&.)
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
import qualified Bookseller1Simple
import qualified Bookseller2HigherOrder
import Choreography (runChoreography)
import Choreography.Choreo (epp)
import Choreography.Network (runNetwork)
import Choreography.Network.Local (mkLocalConfig)
import Data (defaultBudget, deliverable, price, textbooks)
import qualified Data
import TTY (runTTYStateful, TTYEnv (..))

tests :: IO [Test]
tests = return tests'

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
  },

  getNormalPT PropertyTest {
    name = "bookseller-1-simple",
    tags =[],
    property = do book <- elements textbooks  -- The Gen Monad. Doing it this way kinda breaks failure-case-printing :(
                  let locs = ["seller", "buyer"]
                  return $ ioProperty $ do
                      config <- mkLocalConfig locs
                      [delivery] <- nub <$> 
                        mapConcurrently
                        (runChoreography config (Bookseller1Simple.bookseller $ Data.name book))
                        locs
                      case delivery of
                        Nothing -> return $ Bookseller1Simple.budget < price book
                        Just d -> return $ Bookseller1Simple.budget >= price book && d == deliverable book
  },

  getNormalPT PropertyTest {
    name = "bookseller-1-prime",
    tags =[],
    property = do book <- elements textbooks  -- The Gen Monad. Doing it this way kinda breaks failure-case-printing :(
                  let locs = ["seller", "buyer"]
                  return $ ioProperty $ do
                      config <- mkLocalConfig locs
                      [delivery] <- nub <$> 
                        mapConcurrently
                        (runChoreography config (Bookseller1Simple.bookseller' $ Data.name book))
                        locs
                      case delivery of
                        Nothing -> return $ Bookseller1Simple.budget < price book
                        Just d -> return $ Bookseller1Simple.budget >= price book && d == deliverable book
  },

  getNormalPT PropertyTest {
    name = "bookseller-2-higher-order",
    tags =[],
    property = do book <- elements textbooks  -- The Gen Monad. Doing it this way kinda breaks failure-case-printing :(
                  contrib2 <- arbitrary
                  let situation = [ ("seller", [], [])
                                  , ("buyer", [Data.name book], [show $ deliverable book | price book - contrib2 <= Bookseller2HigherOrder.budget])
                                  , ("buyer2", [show @Int contrib2], [])]
                  return $ ioProperty $ do
                      config <- mkLocalConfig [l | (l, _, _) <- situation]
                      results <- mapConcurrently
                                             (\(name, inputs, outputs) -> do (os', _) <- runTTYStateful
                                                                                    inputs
                                                                                    $ runNetwork config name $ epp (
                                                                                        Bookseller2HigherOrder.bookseller Bookseller2HigherOrder.mkDecision2
                                                                                      ) name
                                                                             return (os' === outputs))
                                             situation
                      return $ foldl1 (.&.) results
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
