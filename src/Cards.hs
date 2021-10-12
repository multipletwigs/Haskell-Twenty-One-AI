-- | Implement the card-related types and helper functions
module Cards where

import           Control.DeepSeq

-- | The four base suits
data Suit = Spade | Club | Diamond | Heart
  deriving (Eq, Ord, Enum, Bounded)

instance Show Suit where
    show Spade   = "S"
    show Club    = "C"
    show Diamond = "D"
    show Heart   = "H"

data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
          | Jack | Queen | King
  deriving (Eq, Ord, Enum, Bounded)

instance Show Rank where
    show Ace   = "A"
    show Two   = "2"
    show Three = "3"
    show Four  = "4"
    show Five  = "5"
    show Six   = "6"
    show Seven = "7"
    show Eight = "8"
    show Nine  = "9"
    show Ten   = "T"
    show Jack  = "J"
    show Queen = "Q"
    show King  = "K"

-- | A Card is a suit and a rank
data Card = Card Suit Rank
  deriving (Eq)

instance Show Card where
    show (Card s r) = show s ++ show r

instance NFData Card where
    rnf = rwhnf

instance Ord Card where
    compare (Card s1 r1) (Card s2 r2) | s1 == s2  = compare r1 r2
                                      | otherwise = compare s1 s2

-- | A Hand is a collection of Cards
type Hand = [Card]

{-
    Coversion functions
-}

getRank :: Card -> Rank
getRank (Card _ r) = r
