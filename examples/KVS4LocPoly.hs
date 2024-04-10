{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import Choreography (runChoreography)
import Choreography.Choreo
import Choreography.Location
import Choreography.Network.Http
import Control.Concurrent (threadDelay)
import Control.Monad
import Data.IORef
import Data.Map (Map, (!))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Proxy
import GHC.IORef (IORef (IORef))
import GHC.TypeLits (KnownSymbol)
import System.Environment

client :: Proxy "client"
client = Proxy

primary :: Proxy "primary"
primary = Proxy

backup1 :: Proxy "backup1"
backup1 = Proxy

backup2 :: Proxy "backup2"
backup2 = Proxy

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
type ReplicationStrategy a = Request @ "primary" -> a -> Choreo Participants IO (Response @ "primary")

-- | `nullReplicationStrategy` is a replication strategy that does not replicate the state.
nullReplicationStrategy :: ReplicationStrategy (IORef State @ "primary")
nullReplicationStrategy request stateRef = do
  primary `locally` \unwrap -> case unwrap request of
    Put key value -> do
      modifyIORef (unwrap stateRef) (Map.insert key value)
      return (Just value)
    Get key -> do
      state <- readIORef (unwrap stateRef)
      return (Map.lookup key state)

-- | `doBackup` relays a mutating request to a backup location.
doBackup ::
  (KnownSymbol a,
   KnownSymbol b,
   Member a Participants,
   Member b Participants
   )=>
  Proxy a ->
  Proxy b ->
  Request @ a ->
  IORef State @ b ->
  Choreo Participants IO ()
doBackup locA locB request stateRef = do
  cond (locA, request) \case
    Put _ _ -> do
      request' <- (locA, request) ~> locB
      (locB, \unwrap -> handleRequest (unwrap request') (unwrap stateRef))
        ~~> locA
      return ()
    _ -> do
      return ()

-- | `primaryBackupReplicationStrategy` is a replication strategy that replicates the state to a backup server.
primaryBackupReplicationStrategy :: ReplicationStrategy (IORef State @ "primary", IORef State @ "backup1")
primaryBackupReplicationStrategy request (primaryStateRef, backupStateRef) = do
  -- relay request to backup if it is mutating (= PUT)
  doBackup primary backup1 request backupStateRef

  -- process request on primary
  primary `locally` \unwrap -> handleRequest (unwrap request) (unwrap primaryStateRef)

-- | `doubleBackupReplicationStrategy` is a replication strategy that replicates the state to two backup servers.
doubleBackupReplicationStrategy ::
  ReplicationStrategy
    (IORef State @ "primary", IORef State @ "backup1", IORef State @ "backup2")
doubleBackupReplicationStrategy
  request
  (primaryStateRef, backup1StateRef, backup2StateRef) = do
    -- relay to two backup locations
    doBackup primary backup1 request backup1StateRef
    doBackup primary backup2 request backup2StateRef

    -- process request on primary
    primary `locally` \unwrap ->
      handleRequest (unwrap request) (unwrap primaryStateRef)

-- | `kvs` is a choreography that processes a single request at the client and returns the response.
-- It uses the provided replication strategy to handle the request.
kvs :: Request @ "client" -> a -> ReplicationStrategy a -> Choreo Participants IO (Response @ "client")
kvs request stateRefs replicationStrategy = do
  request' <- (client, request) ~> primary

  -- call the provided replication strategy
  response <- replicationStrategy request' stateRefs

  -- send response to client
  (primary, response) ~> client

-- | `nullReplicationChoreo` is a choreography that uses `nullReplicationStrategy`.
nullReplicationChoreo :: Choreo Participants IO ()
nullReplicationChoreo = do
  stateRef <- primary `locally` \_ -> newIORef (Map.empty :: State)
  loop stateRef
  where
    loop :: IORef State @ "primary" -> Choreo Participants IO ()
    loop stateRef = do
      request <- client `locally` \_ -> readRequest
      response <- kvs request stateRef nullReplicationStrategy
      client `locally` \unwrap -> do putStrLn (show (unwrap response))
      loop stateRef

-- | `primaryBackupChoreo` is a choreography that uses `primaryBackupReplicationStrategy`.
primaryBackupChoreo :: Choreo Participants IO ()
primaryBackupChoreo = do
  primaryStateRef <- primary `locally` \_ -> newIORef (Map.empty :: State)
  backupStateRef <- backup1 `locally` \_ -> newIORef (Map.empty :: State)
  loop (primaryStateRef, backupStateRef)
  where
    loop :: (IORef State @ "primary", IORef State @ "backup1") -> Choreo Participants IO ()
    loop stateRefs = do
      request <- client `locally` \_ -> readRequest
      response <- kvs request stateRefs primaryBackupReplicationStrategy
      client `locally` \unwrap -> do putStrLn (show (unwrap response))
      loop stateRefs

-- | `doubleBackupChoreo` is a choreography that uses `doubleBackupReplicationStrategy`.
doubleBackupChoreo :: Choreo Participants IO ()
doubleBackupChoreo = do
  primaryStateRef <- primary `locally` \_ -> newIORef (Map.empty :: State)
  backup1StateRef <- backup1 `locally` \_ -> newIORef (Map.empty :: State)
  backup2StateRef <- backup2 `locally` \_ -> newIORef (Map.empty :: State)
  loop (primaryStateRef, backup1StateRef, backup2StateRef)
  where
    loop :: (IORef State @ "primary", IORef State @ "backup1", IORef State @ "backup2") -> Choreo Participants IO ()
    loop stateRefs = do
      request <- client `locally` \_ -> readRequest
      response <- kvs request stateRefs doubleBackupReplicationStrategy
      client `locally` \unwrap -> do putStrLn ("> " ++ show (unwrap response))
      loop stateRefs

main :: IO ()
main = do
  [loc] <- getArgs
  case loc of
    "client" -> runChoreography config primaryBackupChoreo "client"
    "primary" -> runChoreography config primaryBackupChoreo "primary"
    "backup1" -> runChoreography config primaryBackupChoreo "backup1"
    "backup2" -> runChoreography config primaryBackupChoreo "backup2"
  return ()
  where
    config =
      mkHttpConfig
        [ ("client", ("localhost", 3000)),
          ("primary", ("localhost", 4000)),
          ("backup1", ("localhost", 5000)),
          ("backup2", ("localhost", 6000))
        ]
