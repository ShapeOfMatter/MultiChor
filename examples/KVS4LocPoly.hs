{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-
# Example: Key-value store with location polymorphism

This is the final version of the 4-stage key-value store tutorial where we define the location-polymorphic choreography `doBackup`. We use it to define `doubleBackupReplicationStrategy`, which replicates data to two backup locations (`backup1` and `backup2`).

## Execution

```bash
# start primary
cabal run kvs4 primary
# on a different terminal, start backup1
cabal run kvs4 backup1
# another terminal for backup2
cabal run kvs4 backup2
# yet another terminal for client
cabal run kvs4 client
GET hello
> Nothing
PUT hello world
> Just "world"
GET hello
> Just "world"
```
-}

module KVS4LocPoly where

import Choreography
import Choreography.Network.Http
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.TypeLits (KnownSymbol)
import System.Environment

$(mkLoc "client")
$(mkLoc "primary")
$(mkLoc "backup1")
$(mkLoc "backup2")
type Participants = ["client", "primary", "backup1", "backup2"]

type State = Map String String

data Request = Put String String | Get String deriving (Show, Read)

type Response = Maybe String

-- | `readRequest` reads a request from the terminal.
readRequest :: IO Request
readRequest = do
  putStrLn "Command?"
  line <- getLine
  case parseRequest line of
    Just t -> return t
    Nothing -> putStrLn "Invalid command" >> readRequest
  where
    parseRequest :: String -> Maybe Request
    parseRequest s =
      let l = words s
       in case l of
            ["GET", k] -> Just (Get k)
            ["PUT", k, v] -> Just (Put k v)
            _ -> Nothing

-- | `handleRequest` handle a request and returns the new the state.
handleRequest :: Request -> IORef State -> IO Response
handleRequest request stateRef = case request of
  Put key value -> do
    modifyIORef stateRef (Map.insert key value)
    return (Just value)
  Get key -> do
    state <- readIORef stateRef
    return (Map.lookup key state)

-- | ReplicationStrategy specifies how a request should be handled on possibly replicated servers
-- `a` is a type that represent states across locations
type ReplicationStrategy a = Located '["primary"] Request -> a -> Choreo Participants IO (Located '["primary"] Response)

-- | `nullReplicationStrategy` is a replication strategy that does not replicate the state.
nullReplicationStrategy :: ReplicationStrategy (Located '["primary"] (IORef State))
nullReplicationStrategy request stateRef = do
  primary `locally` \un -> case un primary request of
    Put key value -> do
      modifyIORef (un primary stateRef) (Map.insert key value)
      return (Just value)
    Get key -> do
      state <- readIORef (un primary stateRef)
      return (Map.lookup key state)

-- | `doBackup` relays a mutating request to a backup location.
doBackup ::
  (KnownSymbol a,
   KnownSymbol b,
   KnownSymbols ps)=>
  Member a ps ->
  Member b ps ->
  Located '[a] Request ->
  Located '[b] (IORef State) ->
  Choreo ps IO ()
doBackup locA locB request stateRef = do
  broadcast (locA, request) >>= \case
    Put _ _ -> do
      request' <- (locA, request) ~> locB @@ nobody
      _ <- (locB, \un -> handleRequest (un singleton request') (un singleton stateRef))
        ~~> locA @@ nobody
      return ()
    _ -> do
      return ()

-- | `primaryBackupReplicationStrategy` is a replication strategy that replicates the state to a backup server.
primaryBackupReplicationStrategy :: ReplicationStrategy (Located '["primary"] (IORef State), Located '["backup1"] (IORef State))
primaryBackupReplicationStrategy request (primaryStateRef, backupStateRef) = do
  -- relay request to backup if it is mutating (= PUT)
  doBackup primary backup1 request backupStateRef

  -- process request on primary
  primary `locally` \un -> handleRequest (un primary request) (un primary primaryStateRef)

-- | `doubleBackupReplicationStrategy` is a replication strategy that replicates the state to two backup servers.
doubleBackupReplicationStrategy ::
  ReplicationStrategy
    (Located '["primary"] (IORef State), Located '["backup1"] (IORef State), Located '["backup2"] (IORef State))
doubleBackupReplicationStrategy
  request
  (primaryStateRef, backup1StateRef, backup2StateRef) = do
    -- relay to two backup locations
    doBackup primary backup1 request backup1StateRef
    doBackup primary backup2 request backup2StateRef

    -- process request on primary
    primary `locally` \un ->
      handleRequest (un primary request) (un primary primaryStateRef)

-- | `kvs` is a choreography that processes a single request at the client and returns the response.
-- It uses the provided replication strategy to handle the request.
kvs :: Located '["client"] Request -> a -> ReplicationStrategy a -> Choreo Participants IO (Located '["client"] Response)
kvs request stateRefs replicationStrategy = do
  request' <- (client, request) ~> primary @@ nobody

  -- call the provided replication strategy
  response <- replicationStrategy request' stateRefs

  -- send response to client
  (primary, response) ~> client @@ nobody

-- | `nullReplicationChoreo` is a choreography that uses `nullReplicationStrategy`.
nullReplicationChoreo :: Choreo Participants IO ()
nullReplicationChoreo = do
  stateRef <- primary `_locally` newIORef (Map.empty :: State)
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
  primaryStateRef <- primary `_locally` newIORef (Map.empty :: State)
  backupStateRef <- backup1 `_locally` newIORef (Map.empty :: State)
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
  primaryStateRef <- primary `_locally` newIORef (Map.empty :: State)
  backup1StateRef <- backup1 `_locally` newIORef (Map.empty :: State)
  backup2StateRef <- backup2 `_locally` newIORef (Map.empty :: State)
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
