module Tests where

import Auction qualified
import Bank2PC qualified
import Bookseller0Network qualified
import Bookseller1Simple qualified
import Bookseller2HigherOrder qualified
import Bookseller3LocPoly qualified
import BooksellerFancy qualified
import CLI (runCLIStateful)
import CardGame qualified
import ChooseTeams qualified
import Choreography
import Choreography.Network.Local (mkLocalConfig)
import Control.Concurrent.Async (mapConcurrently)
import Data (BooksellerArgs (..), reference)
import Data.List (nub, sort)
import Data.Maybe (maybeToList)
import DelegationFig20 qualified
import DiffieHellman qualified
import Distribution.TestSuite (Test)
import Distribution.TestSuite.QuickCheck
import GMWReal qualified
import Karatsuba qualified
import KVS5Fig17 qualified
import KVS6SizePoly qualified
import KVS8Paper qualified
import Lottery qualified
import MPCFake qualified
import ObliviousTransfer qualified
import Playground qualified
import QuickSort qualified
import System.Random (getStdGen, setStdGen, mkStdGen)
import Test.QuickCheck qualified as QC
import Test.QuickCheck
  ( Positive,
    Testable,
    getPositive,
    ioProperty,
    counterexample,
    (===),
    (.&&.)
  )

tests :: IO [Test]
tests = return $ asTest <$> tests'

normalSettings :: TestArgs
normalSettings = stdTestArgs {verbosity = Verbose}

data MyPropertyTest where
  MyPT :: forall prop. (Testable prop) => TestArgs -> PropertyTest prop -> MyPropertyTest

asTest :: MyPropertyTest -> Test
asTest (MyPT args pt) = getPropertyTestWith args pt

quickCheckWith :: QC.Args -> MyPropertyTest -> IO ()
quickCheckWith args (MyPT defaultArgs pt) = QC.quickCheckWith (testArgsToArgs $ argsToTestArgsWith defaultArgs args) (property pt)

quickCheck :: MyPropertyTest -> IO ()
quickCheck (MyPT args pt) = QC.quickCheckWith (testArgsToArgs args) (property pt)

getTestByName :: String -> MyPropertyTest
getTestByName name = case [pt | pt@(MyPT _ PropertyTest{name=n}) <- tests', n == name] of
                           [pt] -> pt
                           [] -> error $ "No such test as " ++ name
                           _ -> error $ "Found multiple tests with name " ++ name

tests' :: [MyPropertyTest]
tests' =
  [ MyPT normalSettings
      PropertyTest
        { name = "tautology",
          tags = [],
          property = \i -> (===) @Int i i
        },
    MyPT normalSettings
      PropertyTest
        { name = "auction",
          tags = [],
          property = \(args :: Auction.Args) -> ioProperty do
            let situation =
                  [ ("b1", [Auction.b1 args]),
                    ("b2", [Auction.b2 args]),
                    ("b3", [Auction.b3 args]),
                    ("b4", [Auction.b4 args]),
                    ("b5", [Auction.b5 args]),
                    ("seller", []),
                    ("proctor", [])
                  ]
            config <- mkLocalConfig [l | (l, _) <- situation]
            [[results]] <- nub <$>
              (mapConcurrently
                ( \(name, inputs) ->
                    fst
                      <$> runCLIStateful
                        (show <$> inputs)
                        (runChoreography config Auction.auction name)
                )
                situation
              )
            let (winner, secondPlaceBid) = read @(LocTm, Auction.Bid) results
                (possibleWinners, referenceBid) = reference args
                correctWinner = counterexample (show (winner, possibleWinners)) (winner `elem` possibleWinners)
            return $ correctWinner .&&. (secondPlaceBid === referenceBid)
        },
    MyPT normalSettings
      PropertyTest
        { name = "bank-2pc",
          tags = [],
          property = \(args@(Bank2PC.Args txns) :: Bank2PC.Args "alice" "bob") -> ioProperty do
            let situation =
                  [ ("client", Bank2PC.render <$> txns),
                    ("coordinator", []),
                    ("alice", []),
                    ("bob", [])
                  ]
            config <- mkLocalConfig [l | (l, _) <- situation]
            results <-
              mapConcurrently
                ( \(name, inputs) ->
                    fst
                      <$> runCLIStateful
                        inputs
                        (runChoreography config Bank2PC.startBank name)
                )
                situation
            return $ results === reference args
        },
    MyPT normalSettings
      PropertyTest
        { name = "bookseller-0-network",
          tags = [],
          property = \args@BooksellerArgs {books, choice, budget} -> ioProperty do
            let situation =
                  [ ("seller", [show books], Bookseller0Network.seller),
                    ("buyer", [show budget, choice], Bookseller0Network.buyer)
                  ]
            config <- mkLocalConfig [l | (l, _, _) <- situation]
            [([], ()), (delivery, ())] <-
              mapConcurrently
                ( \(name, inputs, process) -> runCLIStateful inputs $ runNetwork config name process
                )
                situation
            return $ (read <$> delivery) === maybeToList (reference args)
        },
    MyPT normalSettings
      PropertyTest
        { name = "bookseller-1-simple",
          tags = [],
          property = \args@BooksellerArgs {books, choice, budget} -> ioProperty do
            let situation =
                  [ ("seller", [show books]),
                    ("buyer", [show budget, choice])
                  ]
            config <- mkLocalConfig [l | (l, _) <- situation]
            [([], ()), (delivery, ())] <-
              mapConcurrently
                ( \(name, inputs) -> runCLIStateful inputs $ runChoreography config Bookseller1Simple.bookseller name
                )
                situation
            return $ (read <$> delivery) === maybeToList (reference args)
        },
    MyPT normalSettings
      PropertyTest
        { name = "bookseller-1-prime",
          tags = [],
          property = \args@BooksellerArgs {books, choice, budget} -> ioProperty do
            let situation =
                  [ ("seller", [show books]),
                    ("buyer", [show budget, choice])
                  ]
            config <- mkLocalConfig [l | (l, _) <- situation]
            [([], ()), (delivery, ())] <-
              mapConcurrently
                ( \(name, inputs) ->
                    runCLIStateful inputs $
                      runChoreography config Bookseller1Simple.bookseller' name
                )
                situation
            return $ (read <$> delivery) === maybeToList (reference args)
        },
    MyPT normalSettings
      PropertyTest
        { name = "bookseller-2-higher-order",
          tags = [],
          property = \args@(BooksellerArgs {books, choice, budget}, contrib :: Positive Int) -> ioProperty do
            let situation =
                  [ ("seller", [show books]),
                    ("buyer", [choice, show budget]),
                    ("buyer2", [show $ getPositive contrib])
                  ]
            config <- mkLocalConfig [l | (l, _) <- situation]
            [([], ()), (delivery, ()), ([], ())] <-
              mapConcurrently
                ( \(name, inputs) ->
                    runCLIStateful inputs $
                      runChoreography config (Bookseller2HigherOrder.bookseller Bookseller2HigherOrder.mkDecision2) name
                )
                situation
            return $ (read <$> delivery) === maybeToList (reference args)
        },
    MyPT normalSettings
      PropertyTest
        { name = "bookseller-2-dummy",
          tags = [],
          property = \(args@BooksellerArgs {books, choice, budget}, contrib :: Positive Int) -> ioProperty do
            let situation =
                  [ ("seller", [show books]),
                    ("buyer", [choice, show budget]),
                    ("buyer2", [show $ getPositive contrib]) -- buyer2 doesn't get used
                  ]
            config <- mkLocalConfig [l | (l, _) <- situation]
            [([], ()), (delivery, ()), ([], ())] <-
              mapConcurrently
                ( \(name, inputs) ->
                    runCLIStateful inputs $
                      runChoreography config (Bookseller2HigherOrder.bookseller Bookseller2HigherOrder.mkDecision1) name
                )
                situation
            return $ (read <$> delivery) === maybeToList (reference args)
        },
    MyPT normalSettings
      PropertyTest
        { name = "bookseller-3-locpoly",
          tags = [],
          property = \args@BooksellerArgs {books, choice, budget} -> ioProperty do
            let situation =
                  [ ("seller", [show books]),
                    ("buyer", [show budget, choice])
                  ]
            let buyer :: Member "buyer" '["buyer"] = singleton
            config <- mkLocalConfig [l | (l, _) <- situation]
            [([], ()), (delivery, ())] <-
              mapConcurrently
                ( \(name, inputs) ->
                    runCLIStateful inputs $
                      runChoreography config (Bookseller3LocPoly.bookseller buyer) name
                )
                situation
            return $ (read <$> delivery) === maybeToList (reference args)
        },
    MyPT normalSettings
      PropertyTest
        { name = "bookseller-fancy",
          tags = [],
          property = \args@(BooksellerArgs {books, choice, budget}, contrib :: Positive Int, contrib2 :: Positive Int, contrib3 :: Positive Int) -> ioProperty do
            let situation =
                  [ ("seller", [show books]),
                    ("buyer", [choice, show budget]),
                    ("buyer2", [show $ getPositive contrib]),
                    ("buyer3", [show $ getPositive contrib2]),
                    ("buyer4", [show $ getPositive contrib3])
                  ]
            config <- mkLocalConfig [l | (l, _) <- situation]
            [([], ()), (delivery, ()), ([], ()), ([], ()), ([], ())] <-
              mapConcurrently
                ( \(name, inputs) ->
                    runCLIStateful inputs $
                      runChoreography config (BooksellerFancy.bookseller @["buyer2", "buyer3", "buyer4"] BooksellerFancy.mkDecision2) name
                )
                situation
            return $ (read <$> delivery) === maybeToList (reference args)
        },
    MyPT normalSettings
      PropertyTest
        { name = "card-game",
          tags = [],
          property = \args@(CardGame.Args deck (c1, c2, c3)) -> ioProperty do
            let situation =
                  [ ("dealer", show <$> cycle deck),
                    ("player1", [show c1]),
                    ("player2", [show c2]),
                    ("player3", [show c3])
                  ]
            config <- mkLocalConfig [l | (l, _) <- situation]
            [[], [r1], [r2], [r3]] <-
              mapConcurrently
                ( \(name, inputs) ->
                    fst
                      <$> runCLIStateful
                        inputs
                        (runChoreography config (CardGame.game @'["player1", "player2", "player3"]) name)
                )
                situation
            return $ (read r1, read r2, read r3) === reference args
        },
    MyPT normalSettings
      PropertyTest
        { name = "choose-teams",
          tags = [],
          property = \args@(ChooseTeams.Args (c2, c4)) -> ioProperty do
            let situation =
                  [ ("player1", []),
                    ("player2", [show c2]),
                    ("player3", []),
                    ("player4", [show c4]),
                    ("player5", [])
                  ]
            config <- mkLocalConfig [l | (l, _) <- situation]
            results <-
              mapConcurrently
                ( \(name, inputs) ->
                    fst
                      <$> runCLIStateful
                        inputs
                        (runChoreography config (ChooseTeams.game @'["player1", "player2", "player3", "player4", "player5"]) name)
                )
                situation
            let [r1, r2, r3, r4, r5] = [concatMap read r | r <- results]
            return $ (r1, r2, r3, r4, r5) === reference args
        },
    MyPT normalSettings
      PropertyTest
        { name = "delegation-fig20",
          tags = [],
          property =
            \args@DelegationFig20.Args
               { DelegationFig20.choice = choice,
                 DelegationFig20.aliceQ = aliceQ,
                 DelegationFig20.bobQ = bobQ,
                 DelegationFig20.carrollF = carrollF
               } -> ioProperty do
                let situation =
                      [ ("alice", [show choice, aliceQ]),
                        ("bob", [bobQ]),
                        ("carroll", [carrollF])
                      ]
                config <- mkLocalConfig [l | (l, _) <- situation]
                [aliceR, bobR, carrollR] <-
                  mapConcurrently
                    ( \(name, inputs) ->
                        fst
                          <$> runCLIStateful
                            inputs
                            (runChoreography config DelegationFig20.mainCho name)
                    )
                    situation
                return $
                  DelegationFig20.Result
                    { DelegationFig20.aliceR = aliceR,
                      DelegationFig20.bobR = bobR,
                      DelegationFig20.carrollR = carrollR
                    }
                    === reference args
        },
    MyPT normalSettings
      PropertyTest -- This test is kinda dumb, but I don't know how better to express "correctness" of DHKE.
        { name = "diffie-hellman",
          tags = [],
          property = \() -> ioProperty do
            let situation =
                  [ ("alice", [""]),
                    ("bob", [])
                  ]
            config <- mkLocalConfig [l | (l, _) <- situation]
            [[a], [b]] <-
              mapConcurrently
                ( \(name, inputs) ->
                    fst
                      <$> runCLIStateful
                        inputs
                        (runChoreography config DiffieHellman.diffieHellman name)
                )
                situation
            return $ read @Integer a === read @Integer b
        },
    MyPT normalSettings
      PropertyTest
        { name = "karatsuba-algo",
          tags = [],
          property =
            \args@Karatsuba.Args
               { Karatsuba.n1 = n1,
                 Karatsuba.n2 = n2
               } -> (Karatsuba.referenceAlgorithm n1 n2) === reference args
        },
    MyPT normalSettings
      PropertyTest
        { name = "karatsuba-choreo",
          tags = [],
          property =
            \args@Karatsuba.Args
               { Karatsuba.n1 = n1,
                 Karatsuba.n2 = n2
               } -> ioProperty do
                let situation =
                      [ ("primary", [show n1, show n2]),
                        ("worker1", []),
                        ("worker2", [])
                      ]
                config <- mkLocalConfig [l | (l, _) <- situation]
                [[response], [], []] <-
                  mapConcurrently
                    ( \(name, inputs) ->
                        fst
                          <$> runCLIStateful
                            inputs
                            (runChoreography config Karatsuba.mainChoreo name)
                    )
                    situation
                return $ read response === reference args
        },
    MyPT normalSettings
      PropertyTest
        { name = "kvs-5-fig17",
          tags = [],
          property =
            \args@KVS5Fig17.Args
               { KVS5Fig17.request = request,
                 KVS5Fig17.handler = handler
               } -> ioProperty do
                let situation =
                      [ ("client", [show request]),
                        ("primary", [handler]),
                        ("backup", [])
                      ]
                config <- mkLocalConfig [l | (l, _) <- situation]
                [[response], [], []] <-
                  mapConcurrently
                    ( \(name, inputs) ->
                        fst
                          <$> runCLIStateful
                            inputs
                            (runChoreography config KVS5Fig17.kvs name)
                    )
                    situation
                return $ read response === reference args
        },
    MyPT normalSettings
      PropertyTest
        { name = "kvs-6-sizepoly",
          tags = [],
          property = \(KVS6SizePoly.Args requests) -> ioProperty do
            let situation =
                  [ ("clientAlice", show <$> requests),
                    ("primaryBob", []),
                    ("backup1", []),
                    ("backup2", []),
                    ("backup3", []),
                    ("backup4", []),
                    ("backup5", [])
                  ]
            config <- mkLocalConfig [l | (l, _) <- situation]
            let strategy1 =
                  KVS6SizePoly.naryReplicationStrategy
                    ( explicitMember ::
                        Member
                          "primaryBob"
                          '[ "clientAlice",
                             "primaryBob",
                             "backup1",
                             "backup2",
                             "backup3",
                             "backup4",
                             "backup5"
                           ]
                    )
                    (consSuper $ consSuper refl)
            let client :: Member "clientAlice" '["clientAlice", "primaryBob", "backup1", "backup2", "backup3", "backup4", "backup5"]
                client = explicitMember
            [responsesA, [], [], [], [], [], []] <-
              mapConcurrently
                ( \(name, inputs) ->
                    fst
                      <$> runCLIStateful
                        inputs
                        (runChoreography config (KVS6SizePoly.kvs strategy1 client) name)
                )
                situation
            let strategy2 =
                  KVS6SizePoly.nullReplicationStrategy
                    ( explicitMember ::
                        Member
                          "primaryBob"
                          '[ "clientAlice",
                             "primaryBob",
                             "backup1",
                             "backup2",
                             "backup3",
                             "backup4",
                             "backup5"
                           ]
                    )
            (responsesB, ()) <- runCLIStateful (show <$> requests) $ runChoreo (KVS6SizePoly.kvs strategy2 client)
            return $ responsesA === responsesB
        },
    MyPT normalSettings
      PropertyTest
        { name = "kvs-8-paper",
          tags = [],
          property = ioProperty <$> kvs8Property -- This can fail randombly because the Choreogrphy in question models random failues.
                                                 -- I've tried to make it easier to debug, but I haven't found a way to keep the choreo doing
                                                 -- what it's supposed to, and still have a 0% chance of the test failling...
        },
    MyPT normalSettings
      PropertyTest
        { name = "lottery",
          tags = [],
          property =
            \args@Lottery.Args
               { Lottery.secrets = (c1, c2, c3, c4, c5),
                 Lottery.randomIs = (s1, s2, s3)
               } -> ioProperty do
                let clientProof ::
                      Subset
                        '["client1", "client2", "client3", "client4", "client5"]
                        '[ "client1",
                           "client2",
                           "client3",
                           "client4",
                           "client5",
                           "server1",
                           "server2",
                           "server3",
                           "analyst"
                         ]
                    clientProof = explicitSubset
                    serverProof ::
                      Subset
                        '["server1", "server2", "server3"]
                        '[ "client1",
                           "client2",
                           "client3",
                           "client4",
                           "client5",
                           "server1",
                           "server2",
                           "server3",
                           "analyst"
                         ]
                    serverProof = explicitSubset
                    analystProof ::
                      Member
                        "analyst"
                        '[ "client1",
                           "client2",
                           "client3",
                           "client4",
                           "client5",
                           "server1",
                           "server2",
                           "server3",
                           "analyst"
                         ]
                    analystProof = explicitMember
                let situation =
                      [ ("client1", [show c1]),
                        ("client2", [show c2]),
                        ("client3", [show c3]),
                        ("client4", [show c4]),
                        ("client5", [show c5]),
                        ("server1", [show s1]),
                        ("server2", [show s2]),
                        ("server3", [show s3]),
                        ("analyst", [])
                      ]
                config <- mkLocalConfig [l | (l, _) <- situation]
                [ [],
                  [],
                  [],
                  [],
                  [], -- clients return nothing
                  [],
                  [],
                  [], -- servers return nothing
                  [response]
                  ] <-
                  mapConcurrently
                    ( \(name, inputs) ->
                        fst
                          <$> runCLIStateful
                            inputs
                            (runChoreography config (Lottery.lottery clientProof serverProof analystProof) name)
                    )
                    situation
                return $ read @Lottery.Fp response === reference args
        },
    MyPT normalSettings
      PropertyTest
        { name = "lottery-central-semantics", -- We don't have good controls over the sequencing of party-loops or operations in the central semantics;
        -- but at least it's deterministic and this will notice if it breaks!
          tags = [],
          property =
            \Lottery.Args
               { Lottery.secrets = (c1, c2, c3, c4, c5),
                 Lottery.randomIs = (s1, s2, s3)
               } -> ioProperty do
                let clientProof ::
                      Subset
                        '["client1", "client2", "client3", "client4", "client5"]
                        '[ "client1",
                           "client2",
                           "client3",
                           "client4",
                           "client5",
                           "server1",
                           "server2",
                           "server3",
                           "analyst"
                         ]
                    clientProof = explicitSubset
                    serverProof ::
                      Subset
                        '["server1", "server2", "server3"]
                        '[ "client1",
                           "client2",
                           "client3",
                           "client4",
                           "client5",
                           "server1",
                           "server2",
                           "server3",
                           "analyst"
                         ]
                    serverProof = explicitSubset
                    analystProof ::
                      Member
                        "analyst"
                        '[ "client1",
                           "client2",
                           "client3",
                           "client4",
                           "client5",
                           "server1",
                           "server2",
                           "server3",
                           "analyst"
                         ]
                    analystProof = explicitMember
                    lottery = Lottery.lottery clientProof serverProof analystProof
                let situation =
                      [ ("client1", [show c1]),
                        ("client2", [show c2]),
                        ("client3", [show c3]),
                        ("client4", [show c4]),
                        ("client5", [show c5]),
                        ("server1", [show s1]),
                        ("server2", [show s2]),
                        ("server3", [show s3]),
                        ("analyst", [])
                      ]
                config <- mkLocalConfig [l | (l, _) <- situation]
                [ [],
                  [],
                  [],
                  [],
                  [],
                  [],
                  [],
                  [],
                  [response]
                  ] <-
                  mapConcurrently
                    ( \(name, inputs) ->
                        fst
                          <$> runCLIStateful
                            inputs
                            (runChoreography config lottery name)
                    )
                    situation
                [centralSemanticsResponse] <-
                  fst
                    <$> runCLIStateful
                      [ show c1,
                        show c2,
                        show c3,
                        show c4,
                        show c5,
                        show s1,
                        show s2,
                        show s3
                      ]
                      (runChoreo lottery)
                return $ read @Lottery.Fp response === read centralSemanticsResponse
        },
    MyPT normalSettings
      PropertyTest
        { name = "mpc-fake",
          tags = [],
          property = \args@(MPCFake.Args circuit p1in p2in p3in p4in) -> ioProperty do
            let situation =
                  [ ("trusted3rdParty", []),
                    ("p1", repeat $ show p1in),
                    ("p2", repeat $ show p2in),
                    ("p3", repeat $ show p3in),
                    ("p4", repeat $ show p4in)
                  ]
            config <- mkLocalConfig [l | (l, _) <- situation]
            [[], [r1], [r2], [r3], [r4]] <-
              mapConcurrently
                ( \(name, inputs) ->
                    fst
                      <$> runCLIStateful
                        inputs
                        (runChoreography config (MPCFake.mpc circuit) name)
                )
                situation
            return $ (read r1, read r2, read r3, read r4) === reference args
        },
    MyPT normalSettings
      PropertyTest
        { name = "obliviousTransfer",
          tags = [],
          property = \args@(ObliviousTransfer.Args b1 b2 b3 b4 s1 s2) -> ioProperty do
            let situation =
                  [ ("A", show <$> [b1, b2, b3, b4]),
                    ("B", show <$> [s1, s2])
                  ]
            config <- mkLocalConfig [l | (l, _) <- situation]
            [([], ()), (result, ())] <-
              mapConcurrently
                ( \(name, inputs) ->
                    runCLIStateful inputs $
                      runChoreography config (ObliviousTransfer.otTest @"A" @"B") name
                )
                situation
            return $ (read @Bool <$> result) === reference args
        },
    MyPT normalSettings
      PropertyTest
        { name = "quicksort",
          tags = [],
          property = \args -> ioProperty do
            let situation =
                  [ ("primary", [show args]),
                    ("worker1",  []),
                    ("worker2",  [])
                  ]
            config <- mkLocalConfig [l | (l, _) <- situation]
            [([result], ()), ([], ()), ([], ())] <-
              mapConcurrently
                ( \(name, inputs) ->
                    runCLIStateful inputs $
                      runChoreography config QuickSort.sort name
                )
                situation
            return $ read @[Int] result === sort args
        },
    MyPT normalSettings
      PropertyTest
        { name = "playground",
          tags = [],
          property = \args@(Playground.Args foo bar) -> ioProperty do
            let situation =
                  [ ("alpha", [show foo]),
                    ("beta",  ["", show bar])
                  ]
            config <- mkLocalConfig [l | (l, _) <- situation]
            [([bar'], ()), ([foo'], ())] <-
              mapConcurrently
                ( \(name, inputs) ->
                    runCLIStateful inputs $
                      runChoreography config Playground.choreography name
                )
                situation
            return $ (read bar', read foo') === reference args
        },
    MyPT (stdTestArgs {verbosity = Verbose, maxSuccess = 100, maxSize = 10})
      PropertyTest
        { name = "gmw-real",
          tags = [],
          property = \args@(GMWReal.Args circuit p1in p2in p3in p4in) -> ioProperty do
            let situation =
                  [ ("p1", repeat $ show p1in),
                    ("p2", repeat $ show p2in),
                    ("p3", repeat $ show p3in),
                    ("p4", repeat $ show p4in)
                  ]
            config <- mkLocalConfig [l | (l, _) <- situation]
            [[r1], [r2], [r3], [r4]] <-
              mapConcurrently
                ( \(name, inputs) ->
                    fst
                      <$> runCLIStateful
                        inputs
                        (runChoreography config (GMWReal.mpc circuit) name)
                )
                situation
            return $ (read r1, read r2, read r3, read r4) === reference args
        }
  ]

kvs8Property :: (QC.Fixed (QC.Large Int), KVS8Paper.Args) -> IO QC.Property
kvs8Property = \(QC.Fixed (QC.Large tempGen), args@(KVS8Paper.Args requests)) -> do
            ambientStdGen <- getStdGen
            setStdGen $ mkStdGen tempGen
            let situation =
                  [ ("clientAlice", show <$> requests),
                    ("primaryBob", []),
                    ("backup1", []),
                    ("backup2", []),
                    ("backup3", []),
                    ("backup4", []),
                    ("backup5", [])
                  ]
            config <- mkLocalConfig [l | (l, _) <- situation]
            [_, [endState], [], [], [], [], []] <-
              mapConcurrently
                ( \(name, inputs) ->
                    fst <$> runCLIStateful inputs (runChoreography config (
                        KVS8Paper.kvsRecursive @"clientAlice" @"primaryBob" @'["backup1", "backup2", "backup3", "backup4", "backup5"]
                      ) name)
                )
                situation
            setStdGen ambientStdGen
            return $ read endState === reference args

