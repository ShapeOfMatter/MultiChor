{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-
# Example: Key-value store with higher-order choreography

## Execution

By default, `primaryBackupReplicationStrategy` will be used. Change `mainChoreo` to `nullReplicationChoreo` to use `nullReplicationStrategy`.

```bash
# start primary
cabal run kvs3 primary
# on a different terminal, start backup
cabal run kvs3 backup
# another terminal for client
cabal run kvs3 client
GET hello
> Nothing
PUT hello world
> Just "world"
GET hello
> Just "world"
```
-}

module KVS3HigherOrder where

import Choreography
import Choreography.Network.Http
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import Logic.Propositional (introAnd)
import System.Environment

$(mkLoc "client")
$(mkLoc "primary")
$(mkLoc "backup")
type Participants = ["client", "primary", "backup"]

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
type ReplicationStrategy a =
  Located '["primary"] Request -> a -> Choreo Participants IO (Located '["primary"] Response)

-- | `nullReplicationStrategy` is a replication strategy that does not replicate the state.
nullReplicationStrategy :: ReplicationStrategy (Located '["primary"] (IORef State))
nullReplicationStrategy request stateRef = do
  primary `locally` \un ->
    handleRequest (un primary request) (un primary stateRef)

-- | `primaryBackupReplicationStrategy` is a replication strategy that replicates the state to a backup server.
primaryBackupReplicationStrategy ::
  ReplicationStrategy (Located '["primary"] (IORef State), Located '["backup"] (IORef State))
primaryBackupReplicationStrategy request (primaryStateRef, backupStateRef) = do
  -- relay request to backup if it is mutating (= PUT)
  broadcastCond (primary `introAnd` primary, request) \case
    Put _ _ -> do
      request' <- (primary, request) ~> backup @@ nobody
      _ <- (backup,
        \un ->
          handleRequest (un backup request') (un backup backupStateRef)
        )
        ~~> primary @@ nobody
      return ()
    _ -> do
      return ()

  -- process request on primary
  primary `locally` \un ->
    handleRequest (un primary request) (un primary primaryStateRef)

-- | `kvs` is a choreography that processes a single request at the client and returns the response.
-- It uses the provided replication strategy to handle the request.
kvs ::
  forall a.
  Located '["client"] Request ->
  a ->
  ReplicationStrategy a ->
  Choreo Participants IO (Located '["client"] Response)
kvs request stateRefs replicationStrategy = do
  request' <- (client, request) ~> primary @@ nobody

  -- call the provided replication strategy
  response <- replicationStrategy request' stateRefs

  -- send response to client
  (primary, response) ~> client @@ nobody

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
  backupStateRef <- backup `locally` \_ -> newIORef (Map.empty :: State)
  loop (primaryStateRef, backupStateRef)
  where
    loop :: (Located '["primary"] (IORef State), Located '["backup"] (IORef State)) -> Choreo Participants IO ()
    loop stateRefs = do
      request <- client `_locally` readRequest
      response <- kvs request stateRefs primaryBackupReplicationStrategy
      client `locally_` \un -> do putStrLn ("> " ++ show (un client response))
      loop stateRefs

main :: IO ()
main = do
  [loc] <- getArgs
  case loc of
    "client" -> runChoreography config mainChoreo "client"
    "primary" -> runChoreography config mainChoreo "primary"
    "backup" -> runChoreography config mainChoreo "backup"
    _ -> error "unknown party"
  return ()
  where
    mainChoreo = primaryBackupChoreo -- or `nullReplicationChoreo`
    config =
      mkHttpConfig
        [ ("client", ("localhost", 3000)),
          ("primary", ("localhost", 4000)),
          ("backup", ("localhost", 5000))
        ]
