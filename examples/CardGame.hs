{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}


module CardGame where

import Choreography
import CLI
import Data (TestArgs, reference)
import Logic.Classes (refl)
import Logic.Propositional (introAnd)
import Test.QuickCheck (Arbitrary, arbitrary, listOf1)



_modulo :: Int -> Int
_modulo = (`mod` 21)
newtype Card = Card_ Int deriving (Eq, Ord, Read, Show)
card :: Int -> Card
card = Card_ . _modulo
instance Num Card where
  (Card_ a) + (Card_ b) = card $ a + b
  (Card_ a) * (Card_ b) = card $ a * b
  abs = id
  signum = const 1
  fromInteger = card . fromInteger
  negate (Card_ a) = card $ (-1) * a
instance Arbitrary Card where
  arbitrary = Card_ <$> arbitrary

type Win = Bool

data Args = Args{ deck :: [Card]
                , choices :: (Bool, Bool, Bool)
                } deriving (Eq, Show, Read)
instance TestArgs Args (Bool, Bool, Bool) where
  reference Args{deck, choices=(c1, c2, c3)} =
    let h11 : h21 : h31 : deck1 = cycle deck  -- should just use State
        (h12, deck12) = if c1 then ([h11, head deck1], tail deck1) else ([h11], deck1)
        (h22, deck22) = if c2 then ([h21, head deck12], tail deck12) else ([h21], deck12)
        (h32, deck32) = if c3 then ([h31, head deck22], tail deck22) else ([h31], deck22)
        common = head deck32
        [win1, win2, win3] = (> card 19) . sum . (common :) <$> [h12, h22, h32]
    in (win1, win2, win3)
instance Arbitrary Args where
  arbitrary = Args <$> listOf1 arbitrary <*> arbitrary

{- The game is very simple, a bit like black-jack.
 - The dealer gives everyone a card, face up.
 - Each player asks for another card, or not.
 - Then the dealer reveals one more card that applies to everyone.
 - Each player individually wins or looses; they win if the sum of their cards (modulo 21) is greater than 19.
 -}
game :: forall players m. (KnownSymbols players) => Choreo ("dealer"': players) (CLI m) ()
game = do
  let players :: Subset players ("dealer" ': players)
      players = consSuper refl
      dealer :: forall ps. (ExplicitMember "dealer" ps) => Member "dealer" ps
      dealer = explicitMember
  hand1 <- fanOut players \player -> do
      card1 <- dealer `_locally` getInput ("Enter random card for " ++ toLocTm player)
      (dealer `introAnd` dealer, card1) ~> inSuper players player @@ nobody
  onTheTable <- fanIn players players \player -> do
      (player `introAnd` inSuper players player, hand1) ~> players
  wantsNextCard <- players `parallel` \player un -> do
      putNote $ "My first card is: " ++ show (un player hand1)
      putNote $ "Cards on the table: " ++ show (un player onTheTable)
      getInput "I'll ask for another? [True/False]"
  hand2 <- fanOut players \player -> do
      let qAddress = inSuper players player
      choice <- (player `introAnd` qAddress, wantsNextCard) ~> dealer @@ qAddress @@ nobody
      flatten (consSuper refl `introAnd` refl) <$>
        cond (refl `introAnd` (dealer @@ qAddress @@ nobody), choice) \case
            True -> do cd2 <- dealer `_locally` getInput (toLocTm player ++ " wants another card:")
                       card2 <- (dealer `introAnd` dealer, cd2) ~> consSuper refl
                       consSuper refl `replicatively` (\un -> [un refl $ localize player hand1
                                                              ,un refl card2])
            False -> consSuper refl `replicatively` (\un -> [un refl $ localize player hand1])
  tblCrd <- dealer `_locally` getInput "Enter a single card for everyone:"
  tableCard <- (dealer `introAnd` dealer, tblCrd) ~> players
  _ <- players `parallel` (\player un -> do
      let hand = un player tableCard : un player hand2
      putNote $ "My hand: " ++ show hand
      putOutput "My win result:" $ sum hand > card 19
    )
  pure ()

