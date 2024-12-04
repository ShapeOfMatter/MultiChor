{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-
# Example: Simple client-server key-value store

This is the first version of the 4-stage key-value store tutorial and implements a simple client-server key-value store. The client can `PUT` a key-value pair to the server and `GET` a value for a given key.

## Execution

```bash
# start server
cabal run kvs1 server
# on a different terminal for client
cabal run kvs1 client
GET hello
> Nothing
PUT hello world
> Just "world"
GET hello
> Just "world"
```
-}

module KVS_PaperExamples where

import Choreography
import Choreography.Network.Http
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import System.Environment

$(mkLoc "client")
$(mkLoc "server")
$(mkLoc "primary")
$(mkLoc "backup")

type Participants2 = ["client", "primary", "backup"]

type Participants = ["client", "server"]

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

-- | `kvs` is a choreography that processes a single request located at the client and returns the response.
kvs ::
  Located '["client"] Request ->
  Located '["server"] (IORef State) ->
  Choreo Participants IO (Located '["client"] Response)
kvs request stateRef = do
  -- send the request to the server
  request' <- (client, request) ~> server @@ nobody
  -- the server handles the response and creates a response
  response <-
    server `locally` \un ->
      handleRequest (un server request') (un server stateRef)
  -- send the response back to the client
  (server, response) ~> client @@ nobody

handlePutBackup ::
  Located '["backup"] (IORef State) ->
  Request ->
  Choreo Participants2 IO ()
handlePutBackup backupStateRef nakedRequest = case nakedRequest of
  -- if the request is a `PUT`, forward the request to the backup node
  Put _ _ -> do
    -- request'' <- locally \un -> nakedRequest -- (primary, request') ~> backup @@ nobody
    ack <-
      backup `locally` \un -> do
        handleRequest nakedRequest (un backup backupStateRef)
    _ <- (backup, ack) ~> primary @@ nobody
    return ()
  _ -> do
    return ()

-- | `kvs` is a choreography that processes a single request located at the client and returns the response.
-- If the request is a `PUT`, it will forward the request to the backup node.
kvs2 ::
  Located '["client"] Request ->
  (Located '["primary"] (IORef State), Located '["backup"] (IORef State)) ->
  Choreo Participants2 IO (Located '["client"] Response)
kvs2 request (primaryStateRef, backupStateRef) = do
  -- send request to the primary node
  request' <- (client, request) ~> primary @@ nobody

  -- branch on the request
  broadcast (primary, request') >>= handlePutBackup backupStateRef

  -- process request on the primary node
  response <-
    primary `locally` \un ->
      handleRequest (un primary request') (un primary primaryStateRef)

  -- send response to client
  (primary, response) ~> client @@ nobody

type Servers = ["primary", "backup"]

servers :: Subset Servers Participants2
servers = primary @@ backup @@ nobody

handlePutBackup3 ::
  Located '["backup"] (IORef State) ->
  Request ->
  Choreo Servers IO ()
handlePutBackup3 backupStateRef nakedRequest = case nakedRequest of
  -- if the request is a `PUT`, forward the request to the backup node
  Put _ _ -> do
    -- request'' <- locally \un -> nakedRequest -- (primary, request') ~> backup @@ nobody
    ack <-
      backup `locally` \un -> do
        handleRequest nakedRequest (un backup backupStateRef)
    _ <- (backup, ack) ~> primary @@ nobody
    return ()
  _ -> do
    return ()

-- | `kvs` is a choreography that processes a single request located at the client and returns the response.
-- If the request is a `PUT`, it will forward the request to the backup node.
kvs3 ::
  Located '["client"] Request ->
  (Located '["primary"] (IORef State), Located '["backup"] (IORef State)) ->
  Choreo Participants2 IO (Located '["client"] Response)
kvs3 request (primaryStateRef, backupStateRef) = do
  -- send request to the primary node
  request' <- (client, request) ~> primary @@ nobody

  -- branch on the request
  _ <- enclave servers $ broadcast (primary, request') >>= handlePutBackup3 backupStateRef

  -- process request on the primary node
  response <-
    primary `locally` \un ->
      handleRequest (un primary request') (un primary primaryStateRef)

  -- send response to client
  (primary, response) ~> client @@ nobody

-- | `mainChoreo` is a choreography that serves as the entry point of the program.
-- It initializes the state and loops forever.
-- HIII :> (*>_*)
mainChoreo :: Choreo Participants IO ()
mainChoreo = do
  stateRef <- server `_locally` newIORef (Map.empty :: State)
  loop stateRef
  where
    loop :: Located '["server"] (IORef State) -> Choreo Participants IO ()
    loop stateRef = do
      request <- client `_locally` readRequest
      response <- kvs request stateRef
      client `locally_` \un -> do putStrLn ("> " ++ show (un client response))
      loop stateRef

main :: IO ()
main = do
  [loc] <- getArgs
  case loc of
    "client" -> runChoreography config mainChoreo "client"
    "server" -> runChoreography config mainChoreo "server"
    _ -> error "unknown party"
  return ()
  where
    config =
      mkHttpConfig
        [ ("client", ("localhost", 3000)),
          ("server", ("localhost", 4000))
        ]
