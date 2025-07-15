module EasyMain where

import Choreography
import Choreography.Network.Http (mkHttpConfig)
import Choreography.Network.Local (mkLocalConfig)
import CLI
import Control.Concurrent.Async (mapConcurrently_)
import System.Environment (getArgs)

easyMain :: forall ps q qs. (ps ~ q ': qs, KnownSymbols ps) => Choreo ps (CLI IO) () -> IO ()
easyMain choreography = do
  args <- getArgs
  case args of
    ["_"] -> runCLIIO $ runChoreo choreography
    ["-"] -> do locConf <- mkLocalConfig parties
                mapConcurrently_ (runCLIIO . runChoreography locConf choreography) parties
    [loc] | loc `elem` parties -> runCLIIO $ runChoreography config choreography loc
    [loc] | otherwise -> error $ "Unknown party " ++ loc
    _ -> error $ "Malformed arguments. Expected \"_\" (central semantics), or \"-\" (all parties in threads), or one of " ++ show parties
  where
    parties = toLocs (refl @ps)
    config = mkHttpConfig $ zipWith (\name port -> (name, ("localhost", port))) parties [5000..]


