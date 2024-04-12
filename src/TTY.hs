{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE DataKinds #-}

-- | This module defines `Choreo`, the monad for writing choreographies.
module TTY where

import Control.Monad.Freer
import Control.Monad.Cont (MonadIO(liftIO))
import Control.Monad.State (StateT(runStateT), MonadState (get, put), lift)


-- | Effect signature for the `Choreo` monad. @m@ is a monad that represents

data TTYSig m a where
  GetLn :: TTYSig m String

  PutLn :: String -> TTYSig m ()

  Internal :: m a -> TTYSig m a

-- | Monad for writing choreographies.
type TTY m = Freer (TTYSig m)

instance (MonadIO m) => MonadIO (TTY m) where 
  liftIO a = toFreer $ Internal $ liftIO a

-- | Run a `Choreo` monad directly.
runTTYIO :: TTY IO a -> IO a
runTTYIO = interpFreer handler
  where
    handler :: TTYSig IO a -> IO a
    handler GetLn  = getLine
    handler (PutLn l) = putStrLn l
    handler (Internal m) = m


getln :: TTY m String
getln = toFreer GetLn

putln :: String -> TTY m ()
putln l = toFreer (PutLn l)

data TTYEnv = TTYEnv {
    inputs :: [String],
    outputs :: [String]
}

runTTYStateful :: forall m a. (MonadFail m, MonadIO m) => [String] -> TTY m a -> m ([String], a)
runTTYStateful ins tma = do (a, e) <- runStateT stateful TTYEnv{inputs = ins, outputs = []}
                            return (outputs e, a)
                         where stateful :: StateT TTYEnv m a
                               stateful = interpFreer handler tma
                               handler :: forall b. TTYSig m b -> StateT TTYEnv m b
                               handler GetLn = do env@TTYEnv{inputs = ln : lns} <- get
                                                  put env{inputs = lns}
                                                  return ln
                               handler (PutLn o) = do env@TTYEnv{outputs = os} <- get
                                                      put env{outputs = o:os}
                               handler (Internal m) = lift m
