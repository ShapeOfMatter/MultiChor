{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

{-
# This example was carried over from earlier work, likely HasChor. It doesn't yet have a unit test attached to it.

# Ring leader election

Experinmental implementaion of ring leader election.
-}

module RingLeader where

import Choreography
import Choreography.Network.Http
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import GHC.TypeLits (KnownSymbol)
import System.Environment

-- an edge of the ring is represented as a tuple of two locaitons l and l' where
-- l is on the left of l'
data Edge g
  = forall l l'.
    (KnownSymbol l, KnownSymbol l') =>
    Edge (Member l g) (Member l' g)

-- a ring is a sequence of edges
type Ring g = [Edge g]

type Label = Int

ringLeader :: forall g. (KnownSymbols g) => Ring g -> Choreo g (StateT Label IO) () -- g for graph
ringLeader r = loop r
  where
    loop :: Ring g -> Choreo g (StateT Label IO) ()
    loop [] = loop r -- not very safe!
    loop (x : xs) = do
      finished <- talkToRight x
      if finished
        then return ()
        else loop xs

    talkToRight :: Edge g -> Choreo g (StateT Label IO) Bool
    talkToRight (Edge left right) = do
      ll <- left `_locally` get
      labelLeft <- (left, ll) ~> right @@ nobody
      labelRight <- right `_locally` get

      finished <-
        right `locally` \un ->
          return $ un singleton labelLeft == un singleton labelRight

      broadcast (right, finished) >>= \case
        True -> do
          right `_locally_` lift (putStrLn "I'm the leader")
          return True
        False -> do
          right `locally_` \un -> put (max (un singleton labelLeft) (un singleton labelRight))
          return False

$(mkLoc "nodeA")
$(mkLoc "nodeB")
$(mkLoc "nodeC")
$(mkLoc "nodeD")

type Participants = ["nodeA", "nodeB", "nodeC", "nodeD"]

ring :: Ring Participants
ring =
  [ Edge nodeA nodeB,
    Edge nodeB nodeC,
    Edge nodeC nodeD,
    Edge nodeD nodeA
  ]

main :: IO ()
main = do
  [loc] <- getArgs
  putStrLn "Please input a label:"
  label <- read <$> getLine
  _ <- runStateT (runChoreography config (ringLeader ring) loc) label
  return ()
  where
    config =
      mkHttpConfig
        [ ("nodeA", ("localhost", 4242)),
          ("nodeB", ("localhost", 4343)),
          ("nodeC", ("localhost", 4444)),
          ("nodeD", ("localhost", 4545))
        ]
