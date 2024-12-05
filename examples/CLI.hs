{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}

-- | This module defines `Choreo`, the monad for writing choreographies.
module CLI where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Freer
import Control.Monad.State (MonadState (get, put), StateT (runStateT), lift)
import Data.Typeable (Typeable, typeRep)
import Text.Read (readMaybe)

type Context = String

data CLISig m a where
  GetStr :: Context -> CLISig m String
  PutStr :: Context -> String -> CLISig m ()
  Internal :: m a -> CLISig m a

type CLI m = Freer (CLISig m)

instance (MonadIO m) => MonadIO (CLI m) where
  liftIO a = toFreer $ Internal $ liftIO a

runCLIIO :: forall m a. (MonadIO m) => CLI m a -> m a
runCLIIO = interpFreer handler
  where
    handler :: CLISig m b -> m b
    handler (GetStr prompt) = liftIO $ putStrLn prompt >> getLine
    handler (PutStr context l) = liftIO $ putStrLn $ context ++ " " ++ l
    handler (Internal m) = m

getstr :: Context -> CLI m String
getstr context = toFreer $ GetStr context

getln :: CLI m String
getln = getstr ""

getInput :: forall a m. (Read a, Typeable a) => Context -> CLI m a
getInput context = do
  str <- getstr context
  case readMaybe str of
    Just a -> return a
    a@Nothing -> error $ "Failed to read \"" ++ str ++ "\" as a " ++ show (typeRep a)

putstr :: Context -> String -> CLI m ()
putstr context l = toFreer $ PutStr context l

putNote :: Context -> CLI m ()
putNote = (`putstr` "")

putOutput :: (Show a) => Context -> a -> CLI m ()
putOutput context a = putstr context $ show a

data TTYEnv = TTYEnv
  { inputs :: [String],
    outputs :: [String]
  }

runCLIStateful :: forall m a. (MonadFail m, MonadIO m) => [String] -> CLI m a -> m ([String], a)
runCLIStateful ins tma = do
  (a, e) <- runStateT stateful TTYEnv {inputs = ins, outputs = []}
  return (reverse $ outputs e, a)
  where
    stateful :: StateT TTYEnv m a
    stateful = interpFreer handler tma
    handler :: forall b. CLISig m b -> StateT TTYEnv m b
    handler (GetStr c) = do
      env@TTYEnv {inputs} <- get
      case inputs of
        ln : lns -> do
          put env {inputs = lns}
          return ln
        [] -> error $ "No input to go with prompt " ++ show c ++ "."
    handler (PutStr _ o) = unless (null o) $ do
      env@TTYEnv {outputs = os} <- get
      put env {outputs = o : os}
    handler (Internal m) = lift m
