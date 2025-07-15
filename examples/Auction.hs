module Auction where

import CLI
import Choreography
import Choreography.Network.Http
import Control.Exception.Base (throwIO)
import Control.Monad (when)
--import Control.Monad.IO.Class (MonadIO (liftIO))
import Data (TestArgs, reference, unsafeQuietHead)
import Data.Foldable (toList)
import Data.Function (on)
import Data.List (groupBy, sortBy)
--import Data.Map (Map)
--import Data.Map qualified as Map
--import GHC.TypeLits (KnownSymbol)
import System.Environment (getArgs)
import System.Random (randomRIO)
import Test.QuickCheck (Arbitrary, arbitrary)

type Buyer1 = "b1"
type Buyer2 = "b2"
type Buyer3 = "b3"
type Buyer4 = "b4"
type Buyer5 = "b5"
type Buyers = '[Buyer1, Buyer2, Buyer3, Buyer4, Buyer5]
type Seller = "seller"
type Proctor = "proctor"
type Participants = Seller ': Proctor ': Buyers
type Bid = Int -- Should probably be Double...


auction :: Choreo Participants (CLI IO) ()
auction = do
  let buyers :: Subset Buyers Participants
      buyers = consSuper consSet
      seller :: Member Seller Participants
      seller = First
      proctor :: Member Proctor Participants
      proctor = Later First
  ownBid <- _parallel buyers (getInput @Bid "Your bid:")
  submittedBids <- gather buyers (seller @@ proctor @@ nobody) ownBid
  orderedBids <- congruently (seller @@ proctor @@ nobody) \un ->
    let bids = un refl submittedBids
        collated = stackLeaves \b -> (toLocTm b, getLeaf bids b)
        sorter a b = compare (snd b) (snd a)
    in sortBy sorter $ toList collated
  winners <- conclaveTo (seller @@ proctor @@ nobody) (First @@ nobody) do
    unique <- congruently' \un ->
      let ((_, winner) : (_, second) : _) = un refl orderedBids
      in winner /= second
    if unique
      then purely First \un -> 
        let (winner : second : _) = un First orderedBids
        in (winner, second)
      else do
        winner' <- locally listedSecond \un -> do
          let winners = fst <$>
                          (unsafeQuietHead $ groupBy (on (==) snd) $ un listedSecond orderedBids)
          (winners !!) <$> randomRIO (0, length winners - 1)
        winner <- broadcast (listedSecond @Proctor, (singleton @Proctor, winner'))
        purely First \un ->
          let ((a, bid) : (b, _) : _) = un First orderedBids
              second = if a == winner then b else a
          in ((winner, bid), (second, bid))
  ((wName, _), (_, sBid)) <- broadcast (seller, (singleton @Seller, winners))
  parallel_ allOf \_ _ -> putOutput "Result:" (wName, sBid)


main :: IO ()
main = do
  [loc] <- getArgs
  when (not $ loc `elem` toLocs (refl @Buyers)) $ throwIO $ userError "unknown party"
  runCLIIO $ runChoreography config auction loc
  where
    urls = repeat "localhost"
    ports = [5000 :: Int ..]
    config = mkHttpConfig $ zip (toLocs (refl @Participants)) (zip urls ports)

data Args = Args
  { b1 :: Bid
  , b2 :: Bid
  , b3 :: Bid
  , b4 :: Bid
  , b5 :: Bid
  }
  deriving (Eq, Show, Read)

instance TestArgs Args ([LocTm], Bid) where
  reference Args {b1, b2, b3, b4, b5} =
    let bids@((_, wBid) : (_, sBid) : _) =
            sortBy (on (flip compare) snd) $
                zip ["b1", "b2", "b3", "b4", "b5"] [b1, b2, b3, b4, b5]
     in ([n | (n, b) <- bids,  b == wBid], sBid)

instance Arbitrary Args where
  arbitrary =
    Args
      <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
