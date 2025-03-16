module Playground where

import CLI (CLI, getInput, putOutput, getstr, runCLIIO, runCLIStateful)
import Choreography
import Choreography.Network.Http (mkHttpConfig)
import Choreography.Network.Local (mkLocalConfig)
import Control.Concurrent.Async (mapConcurrently)
import Data (TestArgs(reference))
import Distribution.TestSuite (Test)
import Distribution.TestSuite.QuickCheck hiding (TestArgs)
import System.Environment (getArgs)
import Test.QuickCheck
  ( Arbitrary,
    arbitrary,
    Testable,
    ioProperty,
    (===),
  )


choreography :: Choreo '["alpha", "beta"] (CLI m) ()
choreography = do
  let alpha :: Member "alpha" '["alpha", "beta"] = listedFirst
      beta  :: Member "beta"  '["alpha", "beta"] = listedSecond
  a <- locally alpha \_ -> getInput @Int "Enter a number for Alpha to send to Beta:"
  _ <- locally beta \_ -> getstr "Press enter for Beta to continute. (Without this you can get problems starting the parties in the wrong order)"
  a' <- (alpha, (singleton @"alpha", a)) ~> beta @@ nobody
  b' <- (beta, \_ -> getInput @String "Enter a word for Beta to send to Alpha (don't forget quotes!):") ~~> alpha @@ nobody
  locally_ alpha \un -> putOutput "Alpha got:" $ un singleton b'
  locally_ beta \un -> putOutput "Beta got:" $ un singleton a'


main :: IO ()
main = do
  [loc] <- getArgs
  _ <- case loc of
    "alpha" -> runCLIIO $ runChoreography config choreography "alpha"
    "beta" -> runCLIIO $ runChoreography config choreography "beta"
    _ -> error "unknown party"
  return ()
  where
    config =
      mkHttpConfig
        [ ("alpha", ("localhost", 5000)),
          ("beta", ("localhost", 5001))
        ]



data Args = Args
  { foo :: Int,
    bar :: String
  }
  deriving (Eq, Show, Read)

instance TestArgs Args (String, Int) where
  reference Args{foo, bar} = (bar, foo)

instance Arbitrary Args where
  arbitrary = Args <$> arbitrary <*> arbitrary

tests :: IO [Test]
tests = return [
    getNormalPT
      PropertyTest
        { name = "tautology",
          tags = [],
          property = \args@Args{foo, bar} -> ioProperty do
            let situation =
                  [ ("alpha", [show foo]),
                    ("beta",  ["", show bar])
                  ]
            config <- mkLocalConfig [l | (l, _) <- situation]
            [([bar'], ()), ([foo'], ())] <-
              mapConcurrently
                ( \(name, inputs) ->
                    runCLIStateful inputs $
                      runChoreography config choreography name
                )
                situation
            return $ (read bar', read foo') === reference args
        }
  ]
  where getNormalPT :: (Testable prop) => PropertyTest prop -> Test
        getNormalPT = getPropertyTestWith stdTestArgs{verbosity = Verbose}

