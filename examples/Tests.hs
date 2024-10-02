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
import qualified CardGame
import qualified ChooseTeams
import qualified DelegationFig20
import qualified DiffieHellman
import qualified KVS5Fig17
import qualified Lottery
import qualified KVS6SizePoly
import qualified MPCFake
import qualified GMWReal
import Choreography
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
                  let buyer :: Member "buyer" '["buyer"] = singleton
                  config <- mkLocalConfig [l | (l, _) <- situation]
                  [ ([], ()), (delivery, ())] <-
                    mapConcurrently (
                      \(name, inputs) -> runCLIStateful inputs $
                        runChoreography config (Bookseller3LocPoly.bookseller buyer) name
                    ) situation
                  return $ (read <$> delivery) === maybeToList (reference args)
  },

  getNormalPT PropertyTest {
    name = "card-game",
    tags =[],
    property = \args@(CardGame.Args deck (c1, c2, c3)) -> ioProperty do
                  let situation = [ ("dealer", show <$> cycle deck)
                                  , ("player1", [show c1])
                                  , ("player2", [show c2])
                                  , ("player3", [show c3])]
                  config <- mkLocalConfig [l | (l, _) <- situation]
                  [[], [r1], [r2], [r3]] <-
                    mapConcurrently (
                      \(name, inputs) -> fst <$> runCLIStateful inputs
                        (runChoreography config (CardGame.game @'["player1", "player2", "player3"]) name)
                    ) situation
                  return $ (read r1, read r2, read r3) === reference args
  },

  getNormalPT PropertyTest {
    name = "choose-teams",
    tags =[],
    property = \args@(ChooseTeams.Args (c2, c4)) -> ioProperty do
                  let situation = [ ("player1", [])
                                  , ("player2", [show c2])
                                  , ("player3", [])
                                  , ("player4", [show c4])
                                  , ("player5", [])]
                  config <- mkLocalConfig [l | (l, _) <- situation]
                  results <-
                    mapConcurrently (
                      \(name, inputs) -> fst <$> runCLIStateful inputs
                        (runChoreography config (ChooseTeams.game @'["player1", "player2", "player3", "player4", "player5"]) name)
                    ) situation
                  let [r1, r2, r3, r4, r5] = [concatMap  read r | r <- results]
                  return $ (r1, r2, r3, r4, r5) === reference args
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
  },

  getNormalPT PropertyTest {
    name = "kvs-5-fig17",
    tags =[],
    property = \args@KVS5Fig17.Args{ KVS5Fig17.request=request
                                   , KVS5Fig17.handler=handler
                                   } -> ioProperty do
                  let situation = [ ("client", [show request])
                                  , ("primary", [handler])
                                  , ("backup", [])]
                  config <- mkLocalConfig [l | (l, _) <- situation]
                  [[response], [], []] <-
                    mapConcurrently (
                      \(name, inputs) -> fst <$> runCLIStateful inputs
                        (runChoreography config KVS5Fig17.kvs name)
                    ) situation
                  return $ read response === reference args
  },

  getNormalPT PropertyTest {
    name = "kvs-6-sizepoly",
    tags =[],
    property = \(KVS6SizePoly.Args requests) -> ioProperty do
                  let situation = [ ("clientAlice", show <$> requests)
                                  , ("primaryBob", [])
                                  , ("backup1", [])
                                  , ("backup2", [])
                                  , ("backup3", [])
                                  , ("backup4", [])
                                  , ("backup5", [])]
                  config <- mkLocalConfig [l | (l, _) <- situation]
                  let strategy1 = KVS6SizePoly.naryReplicationStrategy
                                    (explicitMember :: Member "primaryBob" '["clientAlice", "primaryBob", "backup1", "backup2",
                                                                             "backup3", "backup4", "backup5"])
                                    (consSuper $ consSuper refl)
                  let client :: Member "clientAlice" '["clientAlice", "primaryBob", "backup1", "backup2", "backup3", "backup4", "backup5"]
                      client = explicitMember
                  [responsesA, [], [], [], [], [], []] <-
                    mapConcurrently (
                      \(name, inputs) -> fst <$> runCLIStateful inputs
                        (runChoreography config (KVS6SizePoly.kvs strategy1 client) name)
                    ) situation
                  let strategy2 = KVS6SizePoly.nullReplicationStrategy
                                    (explicitMember :: Member "primaryBob" '["clientAlice", "primaryBob", "backup1", "backup2",
                                                                             "backup3", "backup4", "backup5"])
                  (responsesB, ()) <- runCLIStateful (show <$> requests) $ runChoreo (KVS6SizePoly.kvs strategy2 client)
                  return $ responsesA === responsesB
  },

  getNormalPT PropertyTest {
    name = "lottery",
    tags =[],
    property = \args@Lottery.Args{ Lottery.secrets=(c1, c2, c3, c4, c5)
                                   , Lottery.randomIs=(s1, s2, s3)
                                   } -> ioProperty do
                  let clientProof :: Subset '["client1", "client2", "client3", "client4", "client5"]
                                            '["client1", "client2", "client3", "client4", "client5",
                                              "server1", "server2", "server3", "analyst"]
                      clientProof = explicitSubset
                      serverProof :: Subset '["server1", "server2", "server3"]
                                            '["client1", "client2", "client3", "client4", "client5",
                                              "server1", "server2", "server3", "analyst"]
                      serverProof = explicitSubset
                      analystProof :: Member "analyst"
                                            '["client1", "client2", "client3", "client4", "client5",
                                              "server1", "server2", "server3", "analyst"]
                      analystProof = explicitMember
                  let situation = [ ("client1", [show c1])
                                  , ("client2", [show c2])
                                  , ("client3", [show c3])
                                  , ("client4", [show c4])
                                  , ("client5", [show c5])
                                  , ("server1", [show s1])
                                  , ("server2", [show s2])
                                  , ("server3", [show s3])
                                  , ("analyst", [])
                                  ]
                  config <- mkLocalConfig [l | (l, _) <- situation]
                  [[], [], [], [], [],  -- clients return nothing
                   [], [], [],          -- servers return nothing
                   [response]] <-
                    mapConcurrently (
                      \(name, inputs) -> fst <$> runCLIStateful inputs
                        (runChoreography config (Lottery.lottery clientProof serverProof analystProof) name)
                    ) situation
                  return $ read @Lottery.Fp response === reference args
  },

  getNormalPT PropertyTest {
    name = "lottery-central-semantics", -- We don't have good controls over the sequencing of party-loops or operations in the central semantics;
                                        -- but at least it's deterministic and this will notice if it breaks!
    tags =[],
    property = \Lottery.Args { Lottery.secrets=(c1, c2, c3, c4, c5)
                                   , Lottery.randomIs=(s1, s2, s3)
                                   } -> ioProperty do
                  let clientProof :: Subset '["client1", "client2", "client3", "client4", "client5"]
                                            '["client1", "client2", "client3", "client4", "client5",
                                              "server1", "server2", "server3", "analyst"]
                      clientProof = explicitSubset
                      serverProof :: Subset '["server1", "server2", "server3"]
                                            '["client1", "client2", "client3", "client4", "client5",
                                              "server1", "server2", "server3", "analyst"]
                      serverProof = explicitSubset
                      analystProof :: Member "analyst"
                                            '["client1", "client2", "client3", "client4", "client5",
                                              "server1", "server2", "server3", "analyst"]
                      analystProof = explicitMember
                      lottery = Lottery.lottery clientProof serverProof analystProof
                  let situation = [ ("client1", [show c1])
                                  , ("client2", [show c2])
                                  , ("client3", [show c3])
                                  , ("client4", [show c4])
                                  , ("client5", [show c5])
                                  , ("server1", [show s1])
                                  , ("server2", [show s2])
                                  , ("server3", [show s3])
                                  , ("analyst", [])
                                  ]
                  config <- mkLocalConfig [l | (l, _) <- situation]
                  [[], [], [],
                   [], [], [],
                   [], [], [response]] <-
                    mapConcurrently (
                      \(name, inputs) -> fst <$> runCLIStateful inputs
                        (runChoreography config lottery name)
                    ) situation
                  [centralSemanticsResponse] <- fst <$> runCLIStateful [show c1, show c2, show c3, show c4, show c5,
                                                                        show s1, show s2, show s3]
                                                                       (runChoreo lottery)
                  return $ read @Lottery.Fp response === read centralSemanticsResponse
  },

  getNormalPT PropertyTest {
    name = "mpc-fake",
    tags =[],
    property = \args@(MPCFake.Args circuit p1in p2in p3in p4in) -> ioProperty do
                  let situation = [ ("trusted3rdParty", [])
                                  , ("p1", repeat $ show p1in)
                                  , ("p2", repeat $ show p2in)
                                  , ("p3", repeat $ show p3in)
                                  , ("p4", repeat $ show p4in) ]
                  config <- mkLocalConfig [l | (l, _) <- situation]
                  [[], [r1], [r2], [r3], [r4]] <-
                    mapConcurrently (
                      \(name, inputs) -> fst <$> runCLIStateful inputs
                        (runChoreography config (MPCFake.mpc circuit) name)
                    ) situation
                  return $ (read r1, read r2, read r3, read r4) ===  reference args
  },

  getPropertyTestWith (stdTestArgs{verbosity=Verbose, maxSuccess=100, maxSize=10}) PropertyTest {
    name = "gmw-real",
    tags =[],
    property = \args@(GMWReal.Args circuit p1in p2in p3in p4in) -> ioProperty do
                  let situation = [ ("p1", repeat $ show p1in)
                                  , ("p2", repeat $ show p2in)
                                  , ("p3", repeat $ show p3in)
                                  , ("p4", repeat $ show p4in) ]
                  config <- mkLocalConfig [l | (l, _) <- situation]
                  [[r1], [r2], [r3], [r4]] <-
                    mapConcurrently (
                      \(name, inputs) -> fst <$> runCLIStateful inputs
                        (runChoreography config (GMWReal.mpc circuit) name)
                    ) situation
                  return $ (read r1, read r2, read r3, read r4) ===  reference args
  }

  ]
