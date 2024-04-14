{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-
# Example: Primary-backup key-value store

This is the second version of the 4-stage key-value store tutorial. This builds on the first version and adds a backup location to improve durability.

```bash
# start primary
cabal run kvs2 primary
# on a different terminal, start backup
cabal run kvs2 backup
# another terminal for client
cabal run kvs2 client
GET hello
> Nothing
PUT hello world
> Just "world"
GET hello
> Just "world"
```
-}

module KVS2PrimaryBackup where

import Choreography (runChoreography)
import Choreography.Choreo
import Choreography.Location
import Choreography.Network.Http
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
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

-- | `kvs` is a choreography that processes a single request located at the client and returns the response.
-- If the request is a `PUT`, it will forward the request to the backup node.
kvs ::
  Located "client" Request ->
  (Located "primary" (IORef State), Located "backup" (IORef State)) ->
  Choreo Participants IO (Located "client" Response)
kvs request (primaryStateRef, backupStateRef) = do
  -- send request to the primary node
  request' <- (client, request) ~> primary

  -- branch on the request
  cond (primary, request') \case
    -- if the request is a `PUT`, forward the request to the backup node
    Put _ _ -> do
      request'' <- (primary, request') ~> backup
      ack <-
        backup `locally` \un -> do
          handleRequest (un request'') (un backupStateRef)
      _ <- (backup, ack) ~> primary
      return ()
    _ -> do
      return ()

  -- process request on the primary node
  response <-
    primary `locally` \un ->
      handleRequest (un request') (un primaryStateRef)

  -- send response to client
  (primary, response) ~> client

-- | `mainChoreo` is a choreography that serves as the entry point of the program.
-- It initializes the state and loops forever.
mainChoreo :: Choreo Participants IO ()
mainChoreo = do
  primaryStateRef <- primary `locally` \_ -> newIORef (Map.empty :: State)
  backupStateRef <- backup `locally` \_ -> newIORef (Map.empty :: State)
  loop (primaryStateRef, backupStateRef)
  where
    loop :: (Located "primary" (IORef State), Located "backup" (IORef State)) -> Choreo Participants IO ()
    loop stateRefs = do
      request <- client `_locally` readRequest
      response <- kvs request stateRefs
      client `locally_` \un -> do putStrLn ("> " ++ show (un response))
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
    config =
      mkHttpConfig
        [ ("client", ("localhost", 3000)),
          ("primary", ("localhost", 4000)),
          ("backup", ("localhost", 5000))
        ]
