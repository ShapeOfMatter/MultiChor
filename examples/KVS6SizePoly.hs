{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Logic.Propositional (introAnd)
--import System.Environment
import Text.Read (readMaybe)

readIORef :: MonadIO m => IORef a -> m a
readIORef = liftIO <$> IORef.readIORef
modifyIORef :: MonadIO m => IORef a -> (a -> a) -> m a
modifyIORef ref f = do a <- readIORef ref
                       liftIO $ IORef.modifyIORef ref f
                       return a
newIORef :: MonadIO m => a -> m (IORef a)
newIORef = liftIO <$> IORef.newIORef

$(mkLoc "client")
-- $(mkLoc "primary")
$(mkLoc "backup1")
$(mkLoc "backup2")
type Participants = ["client", "primary", "backup1", "backup2"]

type State = Map String String

data Request = Put String String | Get String deriving (Eq, Ord, Read, Show)

data Response = Found String
              | NotFound
              | Desynchronization [Response]
              deriving (Eq, Ord, Read, Show)

mlookup :: String -> State -> Response
mlookup key = maybe NotFound Found . Map.lookup key

readRequest :: CLI m Request
readRequest = do line <- getstr "Command?"
                 case readMaybe line of
                   Just t -> return t
                   Nothing -> putNote "Invalid command" >> readRequest

-- | PUT returns the old stored value; GET returns whatever was stored.
handleRequest :: (MonadIO m) => IORef State -> Request -> m Response
handleRequest stateRef (Put key value) = mlookup key <$> modifyIORef stateRef (Map.insert key value)
handleRequest stateRef (Get key) = mlookup key <$> readIORef stateRef

data ReplicationStrategy ps m = forall primary others rigging.
  ReplicationStrategy { primary :: Member primary ps
                      , others :: Subset others ps
                      , setup :: Choreo ps m rigging
                      , handle :: forall starts w1.
                                     (Wrapped w1)
                                  => rigging
                                  -> Member primary starts
                                  -> w1 starts Request
                                  -> Choreo ps m (Located '[primary] Response)
                      }


-- | `nullReplicationStrategy` is a replication strategy that does not replicate the state.
nullReplicationStrategy :: (KnownSymbol primary, MonadIO m)
                        => Member primary ps
                        -> ReplicationStrategy ps m
nullReplicationStrategy primary =
  ReplicationStrategy{ primary
                     , others = nobody
                     , setup = primary `_locally` newIORef (Map.empty :: State)
                     , handle = \stateRef pHas request -> primary `locally` \un -> handleRequest (un explicitMember stateRef) (un pHas request)
                     }

naryReplicationStrategy :: (KnownSymbol primary, KnownSymbols backups, MonadIO m)
                        => Member primary ps
                        -> Subset backups ps
                        -> ReplicationStrategy ps m
naryReplicationStrategy primary backups =
  ReplicationStrategy{ primary
                     , others = backups
                     , setup = servers `fanOut` \server -> inSuper servers server `_locally` newIORef (Map.empty :: State)
                     , handle = \stateRef pHas request -> do
                         request' <- (pHas `introAnd` primary, request) ~> servers
                         localResponse <- servers `parallel` \server un -> handleRequest (un server stateRef) (un server request')
                         responses <-fanIn servers undefined \server -> (server `introAnd` inSuper servers server, localResponse) ~> primary @@ nobody
                         (primary @@ nobody) `replicatively` \un -> case nub . un refl $ responses of
                                                                      [r] -> r
                                                                      rs -> Desynchronization rs
                     }
  where servers = primary @@ backups
{-
-- | `kvs` is a choreography that processes a single request at the client and returns the response.
-- It uses the provided replication strategy to handle the request.
kvs :: Located '["client"] Request -> a -> ReplicationStrategy ps m -> Choreo Participants IO (Located '["client"] Response)
kvs request stateRefs replicationStrategy = do
  request' <- (client `introAnd` client, request) ~> primary @@ nobody

  -- call the provided replication strategy
  response <- replicationStrategy request' stateRefs

  -- send response to client
  (primary `introAnd` primary, response) ~> client @@ nobody

-- | `nullReplicationChoreo` is a choreography that uses `nullReplicationStrategy`.
nullReplicationChoreo :: Choreo Participants IO ()
nullReplicationChoreo = do
  stateRef <- primary `locally` \_ -> newIORef (Map.empty :: State)
  loop stateRef
  where
    loop :: Located '["primary"] (IORef State) -> Choreo Participants IO ()
    loop stateRef = do
      request <- client `_locally` readRequest
      response <- kvs request stateRef nullReplicationStrategy
      client `locally_` \un -> do print (un client response)
      loop stateRef

-- | `primaryBackupChoreo` is a choreography that uses `primaryBackupReplicationStrategy`.
primaryBackupChoreo :: Choreo Participants IO ()
primaryBackupChoreo = do
  primaryStateRef <- primary `locally` \_ -> newIORef (Map.empty :: State)
  backupStateRef <- backup1 `locally` \_ -> newIORef (Map.empty :: State)
  loop (primaryStateRef, backupStateRef)
  where
    loop :: (Located '["primary"] (IORef State), Located '["backup1"] (IORef State)) -> Choreo Participants IO ()
    loop stateRefs = do
      request <- client `_locally` readRequest
      response <- kvs request stateRefs primaryBackupReplicationStrategy
      client `locally_` \un -> do print (un client response)
      loop stateRefs

-- | `doubleBackupChoreo` is a choreography that uses `doubleBackupReplicationStrategy`.
doubleBackupChoreo :: Choreo Participants IO ()
doubleBackupChoreo = do
  primaryStateRef <- primary `locally` \_ -> newIORef (Map.empty :: State)
  backup1StateRef <- backup1 `locally` \_ -> newIORef (Map.empty :: State)
  backup2StateRef <- backup2 `locally` \_ -> newIORef (Map.empty :: State)
  loop (primaryStateRef, backup1StateRef, backup2StateRef)
  where
    loop :: (Located '["primary"] (IORef State), Located '["backup1"] (IORef State), Located '["backup2"] (IORef State)) -> Choreo Participants IO ()
    loop stateRefs = do
      request <- client `_locally` readRequest
      response <- kvs request stateRefs doubleBackupReplicationStrategy
      client `locally_` \un -> do putStrLn ("> " ++ show (un client response))
      loop stateRefs

main :: IO ()
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
