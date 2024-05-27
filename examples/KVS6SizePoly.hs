{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
-}

module KVS6SizePoly where

import Choreography
import CLI
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.IORef (IORef)
import qualified Data.IORef as IORef
import Data.List (nub)
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.TypeLits (KnownSymbol)
import Logic.Classes (refl)
import Test.QuickCheck (Arbitrary, arbitrary, listOf, frequency)
import Text.Read (readMaybe)

readIORef :: MonadIO m => IORef a -> m a
readIORef = liftIO <$> IORef.readIORef
modifyIORef :: MonadIO m => IORef a -> (a -> a) -> m a
modifyIORef ref f = do a <- readIORef ref
                       liftIO $ IORef.modifyIORef ref f
                       return a
newIORef :: MonadIO m => a -> m (IORef a)
newIORef = liftIO <$> IORef.newIORef

-- $(mkLoc "client")
-- $(mkLoc "primary")
-- $(mkLoc "backup1")
-- $(mkLoc "backup2")
-- type Participants = ["client", "primary", "backup1", "backup2"]

type State = Map String String

data Request = Put String String
             | Get String
             | Stop
             deriving (Eq, Ord, Read, Show)

data Response = Found String
              | NotFound
              | Stopped
              | Desynchronization [Response]
              deriving (Eq, Ord, Read, Show)

newtype Args = Args [Request] deriving (Eq, Ord, Read, Show)
instance Arbitrary Args where
  arbitrary = do reqs <- pgs
                 return . Args $ reqs ++ [Stop]
    where pgs = listOf $ frequency [ (1, Put <$> arbitrary <*> arbitrary)
                                   , (1, Get <$> arbitrary)
                                   ]


mlookup :: String -> State -> Response
mlookup key = maybe NotFound Found . Map.lookup key

readRequest :: CLI m Request
readRequest = do line <- getstr "Command?"
                 case line of
                   [] -> return Stop
                   _ -> case readMaybe line of
                     Just t -> return t
                     Nothing -> putNote "Invalid command" >> readRequest

-- | PUT returns the old stored value; GET returns whatever was stored.
handleRequest :: (MonadIO m) => IORef State -> Request -> m Response
handleRequest stateRef (Put key value) = mlookup key <$> modifyIORef stateRef (Map.insert key value)
handleRequest stateRef (Get key) = mlookup key <$> readIORef stateRef
handleRequest _         Stop = return Stopped

data ReplicationStrategy ps m = forall primary others rigging. (KnownSymbol primary) =>
  ReplicationStrategy { primary :: Member primary ps
                      , others :: Subset others ps
                      , setup :: Choreo ps m rigging
                      , handle :: forall starts w.
                                     (Wrapped w)
                                  => rigging
                                  -> Member primary starts
                                  -> w starts Request
                                  -> Choreo ps m Response
                      }


-- | `nullReplicationStrategy` is a replication strategy that does not replicate the state.
nullReplicationStrategy :: (KnownSymbol primary, KnownSymbols ps, MonadIO m)
                        => Member primary ps
                        -> ReplicationStrategy ps m
nullReplicationStrategy primary =
  ReplicationStrategy{ primary
                     , others = nobody
                     , setup = primary `_locally` newIORef (Map.empty :: State)
                     , handle = \stateRef pHas request -> (
                           (primary, \un -> handleRequest (un singleton stateRef) (un pHas request)) ~~> refl
                         ) >>= naked refl
                     }

naryReplicationStrategy :: (KnownSymbol primary, KnownSymbols backups, KnownSymbols ps, MonadIO m)
                        => Member primary ps
                        -> Subset backups ps
                        -> ReplicationStrategy ps m
naryReplicationStrategy primary backups =
  ReplicationStrategy{ primary
                     , others = backups
                     , setup = servers `fanOut` \server -> inSuper servers server `_locally` newIORef (Map.empty :: State)
                     , handle = \stateRef pHas request -> do
                         request' <- (primary, (pHas, request)) ~> servers
                         localResponse <- servers `parallel` \server un -> handleRequest (un server stateRef) (un server request')
                         responses <- fanIn servers (primary @@ nobody) \server ->
                                        (server, servers, localResponse) ~> primary @@ nobody
                         response <- (primary @@ nobody) `congruently` \un ->
                           case nub . un refl $ responses of [r] -> r
                                                             rs -> Desynchronization rs
                         ((primary, response) ~> refl) >>= naked refl
                     }
  where servers = primary @@ backups


naryHumans :: (KnownSymbol primary, KnownSymbols backups, KnownSymbols ps, MonadIO m)
                        => Member primary ps
                        -> Subset backups ps
                        -> ReplicationStrategy ps (CLI m)
naryHumans primary backups =
  ReplicationStrategy{ primary
                     , others = backups
                     , setup = primary `_locally` newIORef (Map.empty :: State)
                     , handle = \stateRef pHas request -> do
                         request' <- (primary, (pHas, request)) ~> backups
                         backupResponse <- backups `parallel` \server un -> readResponse (un server request')
                         localResponse <- primary `locally` \un -> handleRequest (un singleton stateRef) (un pHas request)
                         responses <- fanIn backups (primary @@ nobody) \server ->
                           (server, backups, backupResponse) ~> primary @@ nobody
                         response <- (primary @@ nobody) `congruently` \un ->
                           case nub $ un refl localResponse : un refl responses of
                             [r] -> r
                             rs -> Desynchronization rs
                         ((primary, response) ~> refl) >>= naked refl
                     }
  where readResponse :: Request -> CLI m Response
        readResponse r = do line <- getstr $ show r ++ ": "
                            case line of
                              [] -> return NotFound
                              _ -> return $ Found line


kvs :: (KnownSymbol client) => ReplicationStrategy ps (CLI m) -> Member client ps -> Choreo ps (CLI m) ()
kvs ReplicationStrategy{setup, primary, handle} client = do
  rigging <- setup
  let go = do request <- (client, readRequest) -~> primary @@ nobody
              response <- handle rigging singleton request
              case response of
                Stopped -> return ()
                _ -> do client `_locally_` putOutput "Recieved:" response
                        go
  go

{-main :: IO ()
main = do
  [loc] <- getArgs
  case loc of
    "client" -> runChoreography config primaryBackupChoreo "client"
    "primary" -> runChoreography config primaryBackupChoreo "primary"
    "backup1" -> runChoreography config primaryBackupChoreo "backup1"
    "backup2" -> runChoreography config primaryBackupChoreo "backup2"
    _ -> error "unknown party"
  return ()
  where
    config =
      mkHttpConfig
        [ ("client", ("localhost", 3000)),
          ("primary", ("localhost", 4000)),
          ("backup1", ("localhost", 5000)),
          ("backup2", ("localhost", 6000))
        ]

-}
