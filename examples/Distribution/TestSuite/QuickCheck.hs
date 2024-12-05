{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module:       Distribution.TestSuite.QuickCheck
-- Description:  Convert QuickCheck properties into Cabal tests
-- Copyright:    ⓒ Anselm Schüler 2022
-- License:      MIT
-- Maintainer:   Anselm Schüler <mail@anselmschueler.com>
-- Stability:    stable
-- Portability:  Portable
--
-- This module allows you to easily make Cabal tests for the @detailed-0.9@ interface. See the [docs](https://cabal.readthedocs.io/en/3.6/cabal-package.html#example-package-using-detailed-0-9-interface).
-- It sets sensible option declarations for the tests.
--
-- This module re-uses record names from "Distribution.TestSuite" and "Test.QuickCheck".
-- It is recommended that you enable the [@DisambiguateRecordFields@](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/disambiguate_record_fields.html) extension in GHC and/or import the module qualified.
-- For many basic tests, you don’t need to import "Distribution.TestSuite".
--
-- To make a test, simply construct a 'PropertyTest' and call 'getPropertyTest' on it.
--
-- A simple sample test suite:
--
-- @
-- module Tests (tests) where
-- import "Distribution.TestSuite.QuickCheck"
-- import "Test.QuickCheck"
-- tests = [
--   'getPropertyTest' 'PropertyTest' {
--     'name' = /"addition-is-commutative"/,
--     'tags' = [],
--     'property' = \\a b -> a + b 'QC.===' b + a
--     }
--   ]
-- @
--
-- The tests you get as a result support several parameters:
--
-- +--------------------+--------------+---------------------------------------------------------------------------------+
-- | Property name      | Valid values | Effect                                                                          |
-- +====================+==============+=================================================================================+
-- | @silent@           | booleans     | If true, all output is disabled.                                                |
-- |                    |              | Sets 'verbosity' to 'Silent'. See 'QC.chatty'.                                  |
-- |                    |              | Disabling Silent raises the verbosity to Chatty if it is not already higher.    |
-- +--------------------+--------------+---------------------------------------------------------------------------------+
-- | @chatty@           | booleans     | If true, the default amount of output is emitted by QuickCheck.                 |
-- |                    |              | Sets 'verbosity' to 'Chatty'. See 'QC.chatty'.                                  |
-- |                    |              | Note that setting this verbosity option to false does not undo setting it to    |
-- |                    |              | true, but lowers the verbosity by one level if it is not already lower.         |
-- +--------------------+--------------+---------------------------------------------------------------------------------+
-- | @verbose@          | booleans     | If true, prints checked values as output.                                       |
-- |                    |              | Sets 'verbosity' to 'Verbose'. See 'QC.verbose'.                                |
-- |                    |              | Note that setting this verbosity option to false does not undo setting it to    |
-- |                    |              | true, but lowers the verbosity by one level if it is not already lower.         |
-- +--------------------+--------------+---------------------------------------------------------------------------------+
-- | @verboseShrinking@ | booleans     | If true, prints all checked and shrunk values as output.                        |
-- |                    |              | See 'QC.verboseShrinking'.                                                      |
-- +--------------------+--------------+---------------------------------------------------------------------------------+
-- | @verbosity@        | @Silent@,    | Sets the 'verbosity' to the desired level.                                      |
-- |                    | @Chatty@,    |                                                                                 |
-- |                    | or @Verbose@ |                                                                                 |
-- +--------------------+--------------+---------------------------------------------------------------------------------+
-- | @maxDiscardRatio@  | positive     | Maximum number of discarded tests per successful test before giving up.         |
-- |                    | integer      | See 'QC.maxDiscardRatio'.                                                       |
-- +--------------------+--------------+---------------------------------------------------------------------------------+
-- | @noShrinking@      | booleans     | Disables shrinking of test cases.                                               |
-- |                    |              | See 'QC.noShrinking'.                                                           |
-- +--------------------+--------------+---------------------------------------------------------------------------------+
-- | @shrinking@        | booleans     | Opposite of @noShrinking@.                                                      |
-- +--------------------+--------------+---------------------------------------------------------------------------------+
-- | @maxShrinks@       | nonnegative  | Maximum number of shrinks before giving up or zero to disable shrinking.        |
-- |                    | integer      | See 'QC.maxShrinks'.                                                            |
-- +--------------------+--------------+---------------------------------------------------------------------------------+
-- | @maxSuccess@       | positive     | Maximum number of successful tests before succeeding.                           |
-- |                    | integer      | See 'QC.maxSuccess'.                                                            |
-- +--------------------+--------------+---------------------------------------------------------------------------------+
-- | @maxSize@          | positive     | Size to use for the biggest test cases.                                         |
-- |                    | integer      | See 'QC.maxSize'.                                                               |
-- +--------------------+--------------+---------------------------------------------------------------------------------+
-- | @sizeScale@        | positive     | Scales all sizes by a number.                                                   |
-- |                    | integer      | See 'QC.mapSize'.                                                               |
-- +--------------------+--------------+---------------------------------------------------------------------------------+
-- | @replay@           | tuple of     | Replays a previous test case. Pass a string representing a tuple of             |
-- |                    | 'QCGen' and  | the 'QC.usedSeed' and 'QC.usedSize' values of a test case. Use empty string to  |
-- |                    | nonnegative  | disable.                                                                        |
-- |                    | integer or   |                                                                                 |
-- |                    | empty        |                                                                                 |
-- +--------------------+--------------+---------------------------------------------------------------------------------+
--
-- You can set default values by using 'getPropertyTestWith'
-- You can access these values in your test by using 'getPropertyTestUsing'.
-- Do both with 'getPropertyTestWithUsing'.
module Distribution.TestSuite.QuickCheck
  ( -- * Create tests
    getPropertyTest,
    getPropertyTestWith,
    getPropertyTestUsing,
    getPropertyTestWithUsing,
    getPropertyTests,
    propertyTestGroup,

    -- * Argument data types
    PropertyTest (..),
    TestArgs (..),
    Verbosity (..),

    -- * Functions for using arguments
    argsToTestArgs,
    argsToTestArgsWith,
    testArgsToArgs,
    stdTestArgs,
  )
where

import Data.Bool (bool)
import Data.Functor ((<&>))
import qualified Distribution.TestSuite as T
import qualified Test.QuickCheck as QC
import Test.QuickCheck.Random (QCGen)
import Text.Read (readMaybe)

-- | Datatype for setting the verbosity of tests
data Verbosity
  = -- | QuickCheck prints nothing. This sets @'QC.chatty' = 'False'@.
    Silent
  | -- | Print basic statistics. This sets @'QC.chatty' = 'True'@.
    Chatty
  | -- | Print every test case. This applies 'QC.verbose'.
    Verbose
  deriving
    ( Eq,
      -- | 'Silent' < 'Chatty' < 'Verbose'
      Ord,
      Show,
      Read,
      Enum,
      Bounded
    )

-- ! [PARTIAL] This function fails when passed Silent
switchVerbosity :: Verbosity -> Bool -> Verbosity -> Verbosity
switchVerbosity v' True v = v'
switchVerbosity v' False v = min (pred v') v

-- | Arguments for altering property test behaviour.
--   These can be altered in the final Cabal 'T.Test' using 'T.setOption'.
data TestArgs = TestArgs
  { -- | Verbosity for tests. See 'QC.verbose' and 'QC.chatty'.
    verbosity :: Verbosity,
    -- TODO Consider joining verboseShrinking back into verbosity

    -- | Whether QuickCheck should print shrinks. See 'QC.verboseShrinking'.
    verboseShrinking :: Bool,
    -- | Maximum discarded tests per successful test. See 'QC.maxDiscardRatio'.
    maxDiscardRatio :: Int,
    -- | Disable shrinking. See 'QC.noShrinking'.
    noShrinking :: Bool,
    -- | Maximum number of shrink attempts. See 'QC.maxShrinks'.
    maxShrinks :: Int,
    -- | Maximum number of successful checks before passing. See 'QC.maxSuccess'.
    maxSuccess :: Int,
    -- | Maximum size of test cases. See 'QC.maxSize'.
    maxSize :: Int,
    -- | Scale size by an integer using 'QC.mapSize'.
    sizeScale :: Int,
    -- | Replay a previous test. Pass the seed and size given by 'QC.usedSeed' and 'QC.usedSize'.
    replay :: Maybe (QCGen, Int)
  }

-- | Transform a QuickCheck 'QC.Args' value to a 'TestArgs' value, defaulting all missing properties
--
--   @'argsToTestArgs' = 'argsToTestArgsWith' 'stdTestArgs'@
argsToTestArgs :: QC.Args -> TestArgs
argsToTestArgs = argsToTestArgsWith stdTestArgs

-- | Transform a QuickCheck 'QC.Args' value to a 'TestArgs' value, with fallbacks for missing properties given by the first argument.
argsToTestArgsWith :: TestArgs -> QC.Args -> TestArgs
argsToTestArgsWith testArgs QC.Args {..} =
  testArgs
    { verbosity = if chatty then Chatty else Silent,
      maxDiscardRatio,
      maxShrinks,
      maxSuccess,
      maxSize,
      replay
    }

-- | Recover arguments passed to 'QC.quickCheck' from a 'TestArgs'
testArgsToArgs :: TestArgs -> QC.Args
testArgsToArgs TestArgs {..} =
  QC.Args
    { replay,
      maxSuccess,
      maxDiscardRatio,
      maxSize,
      chatty = verbosity >= Chatty,
      maxShrinks
    }

-- | Default arguments for property tests
stdTestArgs :: TestArgs
stdTestArgs =
  TestArgs
    { verbosity = Chatty,
      verboseShrinking = False,
      maxDiscardRatio = 10,
      noShrinking = False,
      maxShrinks = maxBound,
      maxSuccess = 100,
      maxSize = 100,
      sizeScale = 1,
      replay = Nothing
    }

switchVIn :: Verbosity -> Bool -> TestArgs -> TestArgs
switchVIn v' q args@TestArgs {verbosity} = args {verbosity = switchVerbosity v' q verbosity}

setArgStr :: String -> String -> Maybe (TestArgs -> TestArgs)
setArgStr "silent" str =
  readMaybe str <&> \val args@TestArgs {verbosity} ->
    if val
      then args {verbosity = Silent}
      else args {verbosity = max Chatty verbosity}
setArgStr "chatty" str = readMaybe str <&> switchVIn Chatty
setArgStr "verbose" str = readMaybe str <&> switchVIn Verbose
setArgStr "verboseShrinking" str =
  readMaybe str <&> \val args ->
    args {verboseShrinking = val}
setArgStr "verbosity" str =
  readMaybe str <&> \val args ->
    args {verbosity = val}
setArgStr "maxDiscardRatio" str =
  readMaybe str <&> \val args ->
    args {maxDiscardRatio = val}
setArgStr "noShrinking" str =
  readMaybe str <&> \val args ->
    args {noShrinking = val}
setArgStr "shrinking" str =
  readMaybe str <&> \val args ->
    args {noShrinking = not val}
setArgStr "maxShrinks" str =
  readMaybe str <&> \val args ->
    args {maxShrinks = val}
setArgStr "maxSuccess" str =
  readMaybe str <&> \val args ->
    args {maxSuccess = val}
setArgStr "maxSize" str =
  readMaybe str <&> \val args ->
    args {maxSize = val}
setArgStr "sizeScale" str =
  readMaybe str <&> \val args ->
    args {sizeScale = val}
setArgStr "replay" str =
  case str of
    "" -> Just \args -> args {replay = Nothing}
    _ ->
      readMaybe str <&> \val args ->
        args {replay = Just val}
setArgStr _ _ = Nothing

positiveIntType :: T.OptionType
positiveIntType =
  T.OptionNumber
    { optionNumberIsInt = True,
      optionNumberBounds = (Just "1", Nothing)
    }

getOptionDescrs :: TestArgs -> [T.OptionDescr]
getOptionDescrs TestArgs {..} =
  [ T.OptionDescr
      { optionName = "silent",
        optionDescription = "Suppress QuickCheck output",
        optionType = T.OptionBool,
        optionDefault = Just . show $ verbosity == Silent
      },
    T.OptionDescr
      { optionName = "chatty",
        optionDescription = "Print QuickCheck output",
        optionType = T.OptionBool,
        optionDefault = Just . show $ verbosity > Chatty
      },
    T.OptionDescr
      { optionName = "verbose",
        optionDescription = "Print checked values",
        optionType = T.OptionBool,
        optionDefault = Just . show $ verbosity > Verbose
      },
    T.OptionDescr
      { optionName = "verboseShrinking",
        optionDescription = "Print all checked and shrunk values",
        optionType = T.OptionBool,
        optionDefault = Just . show $ verboseShrinking
      },
    T.OptionDescr
      { optionName = "verbosity",
        optionDescription = "Verbosity level",
        optionType = T.OptionEnum ["Silent", "Chatty", "Verbose", "VerboseShrinking"],
        optionDefault = Just $ show verbosity
      },
    T.OptionDescr
      { optionName = "maxDiscardRatio",
        optionDescription = "Maximum number of discarded tests per successful test before giving up",
        optionType = positiveIntType,
        optionDefault = Just $ show maxDiscardRatio
      },
    T.OptionDescr
      { optionName = "noShrinking",
        optionDescription = "Disable shrinking",
        optionType = T.OptionBool,
        optionDefault = Just $ show noShrinking
      },
    T.OptionDescr
      { optionName = "shrinking",
        optionDescription = "Enable shrinking",
        optionType = T.OptionBool,
        optionDefault = Just . show $ not noShrinking
      },
    T.OptionDescr
      { optionName = "maxShrinks",
        optionDescription = "Maximum number of shrinks before giving up or zero to disable shrinking",
        optionType =
          T.OptionNumber
            { optionNumberIsInt = True,
              optionNumberBounds = (Just "0", Nothing)
            },
        optionDefault = Just $ show maxShrinks
      },
    T.OptionDescr
      { optionName = "maxSuccess",
        optionDescription = "Maximum number of successful tests before succeeding",
        optionType = positiveIntType,
        optionDefault = Just $ show maxSuccess
      },
    T.OptionDescr
      { optionName = "maxSize",
        optionDescription = "Size to use for the biggest test cases",
        optionType = positiveIntType,
        optionDefault = Just $ show maxSize
      },
    T.OptionDescr
      { optionName = "sizeScale",
        optionDescription = "Scale all sizes by a number",
        optionType = positiveIntType,
        optionDefault = Just $ show sizeScale
      },
    T.OptionDescr
      { optionName = "replay",
        optionDescription = "Replay a previous test",
        optionType = T.OptionString False,
        optionDefault = Just $ show @(Maybe (QCGen, Int)) Nothing
      }
  ]

getModifiers :: QC.Testable a => TestArgs -> a -> QC.Property
getModifiers TestArgs {verbosity, noShrinking, verboseShrinking, sizeScale} =
  foldr (.) QC.property $
    snd
      <$> filter
        fst
        [ (verbosity == Verbose, QC.verbose),
          (verboseShrinking, QC.verboseShrinking),
          (noShrinking, QC.noShrinking),
          (sizeScale /= 1, QC.mapSize (* sizeScale))
        ]

-- | Property test declaration with metadata
data PropertyTest prop = PropertyTest
  { -- | Name of the test, for Cabal. See See Cabal’s 'T.name'.
    name :: String,
    -- | Tags of the test, for Cabal. See Cabal’s 'T.tags'.
    tags :: [String],
    -- | Property to check. This should usually be or return an instance of 'QC.Testable'.
    property :: prop
  }

qcTestArgs :: QC.Testable a => TestArgs -> a -> IO QC.Result
qcTestArgs args property = QC.quickCheckWithResult (testArgsToArgs args) (getModifiers args property)

-- | Get a Cabal 'T.Test' with custom 'TestArgs' from a 'PropertyTest' that takes the test arguments and returns a 'QC.testable' value
getPropertyTestWithUsing ::
  QC.Testable prop =>
  -- | The arguments for the test
  TestArgs ->
  -- | A property test whose 'property' takes a 'TestArgs' argument
  PropertyTest (TestArgs -> prop) ->
  T.Test
getPropertyTestWithUsing originalArgs PropertyTest {..} =
  let withArgs args =
        T.TestInstance
          { run = do
              result <- qcTestArgs args (property args)
              let resultStr = "\n" ++ show result
              return $ T.Finished case result of
                QC.Success {} -> T.Pass
                QC.GaveUp {} ->
                  T.Error $ "GaveUp: QuickCheck gave up" ++ resultStr
                QC.Failure {} ->
                  T.Fail $ "Failure: A property failed" ++ resultStr
                QC.NoExpectedFailure {} ->
                  T.Fail $ "NoExpectedFailure: A property that should have failed did not" ++ resultStr,
            name,
            tags,
            options = getOptionDescrs originalArgs,
            setOption = \opt str -> case setArgStr opt str of
              Nothing -> Left "Parse error"
              Just f -> Right . withArgs $ f args
          }
   in T.Test $ withArgs originalArgs

-- | Get a Cabal 'T.Test' from a 'PropertyTest' that takes the test arguments and returns a 'QC.Testable' value
getPropertyTestUsing ::
  QC.Testable prop =>
  -- | A property test whose 'property' takes a 'TestArgs' argument
  PropertyTest (TestArgs -> prop) ->
  T.Test
getPropertyTestUsing = getPropertyTestWithUsing stdTestArgs

discardingTestArgs :: PropertyTest prop -> PropertyTest (TestArgs -> prop)
discardingTestArgs test@PropertyTest {property} = test {property = const property}

-- | Get a Cabal 'T.Test' from a 'PropertyTest' with custom 'TestArgs'
getPropertyTestWith ::
  QC.Testable prop =>
  -- | The arguments for the test
  TestArgs ->
  PropertyTest prop ->
  T.Test
getPropertyTestWith args = getPropertyTestWithUsing args . discardingTestArgs

-- | Get a Cabal 'T.Test' from a 'PropertyTest'
getPropertyTest :: QC.Testable prop => PropertyTest prop -> T.Test
getPropertyTest = getPropertyTestWithUsing stdTestArgs . discardingTestArgs

-- | Get a list of 'T.Test's from a list of 'PropertyTest's
getPropertyTests :: QC.Testable prop => [PropertyTest prop] -> [T.Test]
getPropertyTests = (getPropertyTest <$>)

-- | Get a named test group from a list of 'PropertyTest's. These are assumed to be able to run in parallel. See 'T.testGroup' and 'T.Group'.
propertyTestGroup :: QC.Testable prop => String -> [PropertyTest prop] -> T.Test
propertyTestGroup name = T.testGroup name . getPropertyTests
