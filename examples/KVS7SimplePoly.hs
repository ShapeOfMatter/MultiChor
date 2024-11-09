{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module KVS7SimplePoly where

import Choreography
import Data.IORef (IORef, newIORef, readIORef)

type Response = Int
type Key = String
type State = String

errorResponse :: Response
errorResponse = -1

data Request = Get Key
             | Put Key Int
             deriving(Eq, Read, Show)

handleGet :: IORef State -> Key -> IO Response
handleGet s k = do readIORef s >>= putStrLn
                   return $ length k

handlePut :: IORef State -> Key -> Int -> IO Response
handlePut s k v = do readIORef s >>= putStrLn
                     return . fromEnum $ v /= length k

handleRequest :: forall backups. (KnownSymbols backups)
              => Located '["primary"] Request
              -> (Located '["primary"] (IORef State), Faceted backups '[] (IORef State))
              -> Choreo ("primary" ': backups) IO (Located '["primary"] Response)
handleRequest request (primaryStateRef, backupsStateRefs) = broadcast (primary, request) >>= \case
    Put key value -> do oks <- parallel backups \backup un ->
                                   handlePut (viewFacet un backup backupsStateRefs) key value
                        gathered <- gather backups (primary @@ nobody) oks
                        locally primary \un -> if all (== 0) (un primary gathered)
                                                then handlePut (un primary primaryStateRef) key value
                                                else return errorResponse
    Get key -> locally primary \un -> handleGet (un primary primaryStateRef) key
  where primary :: forall ps. Member "primary" ("primary" ': ps)
        primary = listedFirst
        backups = consSuper refl

kvs :: forall backups. (KnownSymbols backups)
    => Located '["client"] Request
    -> (Located '["primary"] (IORef State), Faceted backups '[] (IORef State))
    -> Choreo ("client" ': "primary" ': backups) IO (Located '["client"] Response)
kvs request stateRefs = do
    request' <- (client, request) ~> primary @@ nobody
    response <- enclave (primary @@ backups) (handleRequest request' stateRefs)
    (primary, flatten (First @@ nobody) (First @@ nobody) response) ~> client @@ nobody
  where client :: forall ps. Member "client" ("client" ': ps)
        client = listedFirst
        primary :: forall ps p. Member "primary" (p ': "primary" ': ps)
        primary = listedSecond
        backups = consSuper $ consSuper refl

mainChoreo :: (KnownSymbols backups) => Choreo ("client" ': "primary" ': backups) IO ()
mainChoreo = do
  stateRef <- primary `_locally` newIORef "I'm Primary"
  bStRefs <- parallel backups \p _ -> newIORef ("I'm " ++ toLocTm p)
  loop (stateRef, bStRefs)
  where
    primary :: forall ps p. Member "primary" (p ': "primary" ': ps)
    primary = listedSecond
    client :: forall ps. Member "client" ("client" ': ps)
    client = listedFirst
    backups = consSuper $ consSuper refl
    loop state = do
      request <- _locally client $ read @Request <$> getLine
      response <- kvs request state
      client `locally_` \un -> do putStrLn ("> " ++ show (un client response))
      loop state


main :: IO ()
main = runChoreo (mainChoreo @'["A", "B", "C", "D", "E", "F"])
