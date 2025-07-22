{-# LANGUAGE StandaloneDeriving #-}
module KVS8Paper where

import Prelude hiding (IO)
import Prelude qualified
import CLI
import Choreography
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data (TestArgs, reference)
import Data.Foldable (toList, maximumBy)
import Data.Function (on)
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.List (nub)
import Data.Map (Map)
import Data.Map qualified as Map
import EasyMain (easyMain)
import GHC.TypeLits (KnownSymbol)
import System.Random (randomRIO)
import Test.QuickCheck (Arbitrary, arbitrary, frequency, listOf)
import Text.Read (readMaybe)

type IO = CLI Prelude.IO -- Yes this looks sketchy, but it all checks out. CLI just gives us some mock-able operators for testing.

newEmptyState = liftIO $ IORef.newIORef mempty

updateState ref key value = liftIO do oldState <- IORef.readIORef ref
                                      randomFailure <- (== 1) <$> randomRIO @Int (1, 100)
                                      let useValue = if randomFailure then '_' : value else value
                                      IORef.modifyIORef ref $ Map.insert key useValue
                                      return $ mlookup key oldState

lookupState ref key = liftIO $ mlookup key <$> IORef.readIORef ref

hashState ref = liftIO $ length . show <$> IORef.readIORef ref

mlookup :: String -> State -> Response
mlookup key = maybe NotFound Found . Map.lookup key

readRequest :: IO Request
readRequest = do
  line <- getstr "Command? (uses Read. All strings must be quoted. Leave blank to stop.)"
  case line of
    [] -> return Stop
    _ -> case readMaybe line of
      Just t -> return t
      Nothing -> putNote "Invalid command" >> readRequest

-----------------------------------------------------------------------
data Request = Put String String | Get String | Stop; data Response = Found String | NotFound | Stopped
type State = Map String String

newEmptyState :: IO (IORef State)  -- updateState returns the value previously stored under the key, or
                                   -- `NotFound`. Has a small chance of randomly saving the wrong value!
updateState :: IORef State -> String -> String -> IO Response
lookupState :: IORef State -> String -> IO Response
hashState :: IORef State -> IO Int

kvs :: (KnownSymbol client, KnownSymbol primary,
        KnownSymbols servers, KnownSymbols census) =>
       Member client census ->
       Member primary servers ->
       Subset servers census ->
       Faceted servers '[] (IORef State) ->
       Located '[client] Request ->
       Choreo census IO (Located '[client] Response)
kvs client primary servers stateRefs request = do
  let primary' = inSuper servers primary
  request' <- (client, request) ~> primary' @@ nobody
  requestShared <- (primary', request') ~> servers
  response' <- conclave servers do
    naked allOf requestShared >>= \case
      Put key val -> do
        responses <- parallel allOf ( \sr un -> updateState
                                  (viewFacet un sr stateRefs)
                                  key val )
        _ack <- fanIn (primary @@ nobody) \sr -> do
                  ack <- locally sr \_ -> pure ()
                  (sr, ack) ~> (primary @@ nobody)
        return $ localize primary responses
      Get key -> locally primary ( \un -> lookupState
                             (viewFacet un primary stateRefs)
                             key )
      Stop        -> locally primary \_ -> return Stopped
  response <- (primary',
               flatten (primary @@ nobody) refl response'
              ) ~> client @@ nobody
  _ <- conclave servers do
    naked allOf requestShared >>= \case
      Put _ _ -> do
        hash' <- parallel allOf \sr un ->
                   hashState $ viewFacet un sr stateRefs
        hashes <- gather allOf (primary @@ nobody) hash'
        let check = (1 <) . length . nub . toList
        needsReSynch <- locally primary \un ->
                          pure $ check $ un singleton hashes
        broadcast (primary, needsReSynch) >>= \case
          True -> resynch stateRefs  -- Could take a while!
          False -> return ()
      _ -> return ()
  return response

-----------------------------------------------------------------

kvsRecursive :: forall (client :: LocTy) (primary :: LocTy) (backups :: [LocTy]) servers.
       (KnownSymbol client, KnownSymbol primary, KnownSymbols backups, servers ~ (primary ': backups)) =>
       Choreo (client ': servers) IO ()
kvsRecursive = do
  let client = First @(client ': servers)
      primary = First @servers
      servers = consSuper @servers @servers @client allOf
  stateRefs <- servers `_parallel` newEmptyState
  let go = do request <- _locally client readRequest
              response <- kvs client primary servers stateRefs request
              broadcast (client, response) >>= \case
                Stopped -> return ()
                response' -> do _locally_ client $ putOutput "Received:" response'
                                go
  go
  locally_ (inSuper servers primary) \un -> (liftIO $ IORef.readIORef $ viewFacet un primary stateRefs) >>= putOutput "Ending state:"

resynch :: (KnownSymbols servers) => Faceted servers '[] (IORef State) -> Choreo servers IO ()
resynch stateRefs = do
  allStates <- fanIn allOf \sr -> (sr, \un -> liftIO $ IORef.readIORef (viewFacet un sr stateRefs)) ~~> allOf
  votedState <- congruently allOf \un -> let asBallots = (fmap (`Map.singleton` (1::Int))) <$> un allOf allStates
                                             asMapToVotes = foldr (Map.unionWith (Map.unionWith (+))) mempty asBallots
                                        in fst . maximumBy (compare `on` snd) . Map.toList <$> asMapToVotes
  -- If we want to model the possibility that this is also failable, then we could do that too...
  parallel_ allOf \sr un -> liftIO $ IORef.writeIORef (viewFacet un sr stateRefs) (un sr votedState)

deriving instance Eq Request
deriving instance Ord Request
deriving instance Read Request
deriving instance Show Request
deriving instance Eq Response
deriving instance Ord Response
deriving instance Read Response
deriving instance Show Response



newtype Args = Args [Request] deriving (Eq, Ord, Read, Show)

instance TestArgs Args State where
  reference (Args reqs) = let tuples = [(k, v) | Put k v <- reqs]
                          in Map.fromList tuples

instance Arbitrary Args where
  arbitrary = do
    reqs <- pgs
    return . Args $ reqs ++ [Stop]
    where
      pgs =
        listOf $
          frequency
            [ (1, Put <$> arbitrary <*> arbitrary),
              (1, Get <$> arbitrary)
            ]


type Backups = '[
     "b1"
    --,"b2"
    --,"b3"
    --,"b4"
  ]

main :: Prelude.IO ()
main = easyMain $ do parallel_ (allOf)  -- This step prevents problems with the order in which the clients come online.
                               (\p _ -> void $ getstr ("Press enter to indicate " ++ toLocTm p ++ " is ready:"))
                     kvsRecursive @"client" @"primary" @Backups

