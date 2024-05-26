{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-
This is an implementation of the choreogrpahy shown in fig17 of We Know I Know You Know.
-}

module KVS5Fig17 where

import Choreography
import Choreography.Network.Http
import CLI
import Data (TestArgs, reference)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Logic.Classes (refl)
import System.Environment
import Test.QuickCheck (Arbitrary, arbitrary, elements)

$(mkLoc "client")
$(mkLoc "primary")
$(mkLoc "backup")
type Servers = ["primary", "backup"]
type Participants = "client" ': Servers
servers :: Subset Servers Participants
servers = primary @@ backup @@ nobody

data Request = Put String String | Get String deriving (Eq, Show, Read)
instance Arbitrary Request where
  arbitrary = (\case
      Nothing -> Get
      Just s -> Put s) <$> arbitrary <*> arbitrary

type Response = String

data Args = Args{ request :: Request
                , handler :: String
                } deriving (Eq, Show, Read)
instance TestArgs Args Response where
  reference Args{request, handler} =
    let f = fromMaybe defaultHandler $ handler `lookup` handlers
    in handleRequest f request
instance Arbitrary Args where
  arbitrary = Args <$> arbitrary
                   <*> elements (fst <$> handlers)

handlers :: [(String, String -> String)]
handlers = [ ("reverse", reverse)
           , ("alphabetize", sort)
           ]
defaultHandler :: String -> String
defaultHandler = const "No Handler"

-- | `handleRequest` handle a request. Since we don't have a way of locking paralell state, this is a mock.
handleRequest :: (String -> Response) -> Request -> Response
handleRequest handler request = case request of
  Put key value -> show key ++ " saved as " ++ show value ++ "."
  Get key -> handler key

setup :: Choreo Servers (CLI m) (Located Servers (Request -> Response))
setup = do handlerName <- (primary, \_ -> getstr "How should we mock `Get` Requests? (reverse or alphabetize)")
             ~~> primary @@ backup @@ nobody
           primary @@ backup @@ nobody `congruently` \un -> handleRequest (fromMaybe defaultHandler $ un refl handlerName `lookup` handlers)

-- | `kvs` is a choreography that processes a single request located at the client and returns the response.
-- If the request is a `PUT`, it will forward the request to the backup node.
kvs :: Choreo Participants (CLI m) ()
kvs  = do
  handler <- enclaveToAll servers setup
  request <- (client, \_ -> getInput "Enter the `read`able Request:") ~~> primary @@ backup @@ nobody
  response <- primary @@ backup @@ nobody `congruently` \un -> un refl handler $ un refl request
  response' <- (primary, response) ~> client @@ nobody
  client `locally_` \un -> putOutput "Recieved:" $ un client response'

main :: IO ()
main = do
  [loc] <- getArgs
  case loc of
    "client" -> runCLIIO $ runChoreography config kvs "client"
    "primary" -> runCLIIO $ runChoreography config kvs "primary"
    "backup" -> runCLIIO $ runChoreography config kvs "backup"
    _ -> error "unknown party"
  return ()
  where
    config =
      mkHttpConfig
        [ ("client", ("localhost", 3000)),
          ("primary", ("localhost", 4000)),
          ("backup", ("localhost", 5000))
        ]
