module Playground where

import CLI (CLI, getInput, putOutput, getstr)
import Choreography
import Data (TestArgs(reference))
import EasyMain (easyMain)
import Test.QuickCheck
  ( Arbitrary,
    arbitrary,
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
main = easyMain choreography



data Args = Args
  { foo :: Int,
    bar :: String
  }
  deriving (Eq, Show, Read)

instance TestArgs Args (String, Int) where
  reference Args{foo, bar} = (bar, foo)

instance Arbitrary Args where
  arbitrary = Args <$> arbitrary <*> arbitrary
