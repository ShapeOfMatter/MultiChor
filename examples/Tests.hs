module Tests  where

import Control.Concurrent.Async (mapConcurrently)
import Data.Maybe (maybeToList)
import Distribution.TestSuite (Test)
import Distribution.TestSuite.QuickCheck
import Test.QuickCheck ( (===)
                       , ioProperty
                       , getPositive
                       , Positive
                       , Testable)

import qualified Bank2PC
import qualified Bookseller0Network
import qualified Bookseller1Simple
import qualified Bookseller2HigherOrder
import qualified Bookseller3LocPoly
import qualified DelegationFig20
import qualified DiffieHellman
import Choreography (runChoreography)
import Choreography.Location (explicitMember, Member)
import Choreography.Network (runNetwork)
import Choreography.Network.Local (mkLocalConfig)
import Data (BooksellerArgs(..), reference)
import CLI (runCLIStateful)

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
    property = \args@BooksellerArgs{books, choice, budget} -> ioProperty do
                  let situation = [ ("seller", [show books], Bookseller0Network.seller)
                                  , ("buyer", [show budget, choice], Bookseller0Network.buyer)]
                  config <- mkLocalConfig [l | (l, _, _) <- situation]
                  [([], ()), (delivery, ())] <-
                    mapConcurrently (
                      \(name, inputs, process) -> runCLIStateful inputs $ runNetwork config name process
                    ) situation
                  return $ (read <$> delivery) === maybeToList (reference args)
  },

  getNormalPT PropertyTest {
    name = "bookseller-1-simple",
    tags =[],
    property = \args@BooksellerArgs{books, choice, budget} -> ioProperty do
                  let situation = [ ("seller", [show books])
                                  , ("buyer", [show budget, choice])]
                  config <- mkLocalConfig [l | (l, _) <- situation]
                  [([], ()), (delivery, ())] <-
                    mapConcurrently (
                      \(name, inputs) -> runCLIStateful inputs $ runChoreography config Bookseller1Simple.bookseller name
                    ) situation
                  return $ (read <$> delivery) === maybeToList (reference args)
  },

  getNormalPT PropertyTest {
    name = "bookseller-1-prime",
    tags =[],
    property = \args@BooksellerArgs{books, choice, budget} -> ioProperty do
                  let situation = [ ("seller", [show books])
                                  , ("buyer", [show budget, choice])]
                  config <- mkLocalConfig [l | (l, _) <- situation]
                  [([], ()), (delivery, ())] <-
                    mapConcurrently (
                      \(name, inputs) -> runCLIStateful inputs $
                        runChoreography config Bookseller1Simple.bookseller' name
                    ) situation
                  return $ (read <$> delivery) === maybeToList (reference args)
  },

  getNormalPT PropertyTest {
    name = "bookseller-2-higher-order",
    tags =[],
    property = \args@(BooksellerArgs{books, choice, budget}, contrib :: Positive Int) -> ioProperty do
                  let situation = [ ("seller", [show books])
                                  , ("buyer", [choice, show budget])
                                  , ("buyer2", [show $ getPositive contrib])]
                  config <- mkLocalConfig [l | (l, _) <- situation]
                  [ ([], ()), (delivery, ()), ([], ())] <-
                    mapConcurrently (
                      \(name, inputs) -> runCLIStateful inputs $
                        runChoreography config (Bookseller2HigherOrder.bookseller Bookseller2HigherOrder.mkDecision2) name
                    ) situation
                  return $ (read <$> delivery) === maybeToList (reference args)
  },

  getNormalPT PropertyTest {
    name = "bookseller-2-dummy",
    tags =[],
    property = \(args@BooksellerArgs{books, choice, budget}, contrib :: Positive Int) -> ioProperty do
                  let situation = [ ("seller", [show books])
                                  , ("buyer", [choice, show budget])
                                  , ("buyer2", [show $ getPositive contrib])]  -- buyer2 doesn't get used
                  config <- mkLocalConfig [l | (l, _) <- situation]
                  [ ([], ()), (delivery, ()), ([], ())] <-
                    mapConcurrently (
                      \(name, inputs) -> runCLIStateful inputs $
                        runChoreography config (Bookseller2HigherOrder.bookseller Bookseller2HigherOrder.mkDecision1) name
                    ) situation
                  return $ (read <$> delivery) === maybeToList (reference args)
  },

  getNormalPT PropertyTest {
    name = "bookseller-3-locpoly",
    tags =[],
    property = \args@BooksellerArgs{books, choice, budget} -> ioProperty do
                  let situation = [ ("seller", [show books])
                                  , ("buyer", [show budget, choice])]
                  let buyer :: Member "buyer" '["buyer"] = explicitMember
                  config <- mkLocalConfig [l | (l, _) <- situation]
                  [ ([], ()), (delivery, ())] <-
                    mapConcurrently (
                      \(name, inputs) -> runCLIStateful inputs $
                        runChoreography config (Bookseller3LocPoly.bookseller buyer) name
                    ) situation
                  return $ (read <$> delivery) === maybeToList (reference args)
  },

  getNormalPT PropertyTest {
    name = "bank-2pc",
    tags =[],
    property = \(args@(Bank2PC.Args txns) :: Bank2PC.Args "alice" "bob") -> ioProperty do
                  let situation = [ ("client", Bank2PC.render <$> txns)
                                  , ("coordinator", [])
                                  , ("alice", [])
                                  , ("bob", [])]
                  config <- mkLocalConfig [l | (l, _) <- situation]
                  results <-
                    mapConcurrently (
                      \(name, inputs) -> fst <$> runCLIStateful inputs
                        (runChoreography config Bank2PC.startBank name)
                    ) situation
                  return $ results === reference args
  },

  getNormalPT PropertyTest {
    name = "delegation-fig20",
    tags =[],
    property = \args@DelegationFig20.Args{ DelegationFig20.choice=choice
                                         , DelegationFig20.aliceQ=aliceQ
                                         , DelegationFig20.bobQ=bobQ
                                         , DelegationFig20.carrollF=carrollF
                                         } -> ioProperty do
                  let situation = [ ("alice", [show choice, aliceQ])
                                  , ("bob", [bobQ])
                                  , ("carroll", [carrollF])]
                  config <- mkLocalConfig [l | (l, _) <- situation]
                  [aliceR, bobR, carrollR] <-
                    mapConcurrently (
                      \(name, inputs) -> fst <$> runCLIStateful inputs
                        (runChoreography config DelegationFig20.mainCho name)
                    ) situation
                  return $ DelegationFig20.Result{ DelegationFig20.aliceR=aliceR
                                                 , DelegationFig20.bobR=bobR
                                                 , DelegationFig20.carrollR=carrollR
                                                 } === reference args
  },

  getNormalPT PropertyTest {  -- This test is kinda dumb, but I don't know how better to express "correctness" of DHKE.
    name = "diffie-hellman",
    tags =[],
    property = \() -> ioProperty do
                  let situation = [ ("alice", [""])
                                  , ("bob", [])]
                  config <- mkLocalConfig [l | (l, _) <- situation]
                  [[a], [b]] <-
                    mapConcurrently (
                      \(name, inputs) -> fst <$> runCLIStateful inputs
                        (runChoreography config DiffieHellman.diffieHellman name)
                    ) situation
                  return $ read @Integer a === read @Integer b
  }
  ]

