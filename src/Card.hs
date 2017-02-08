module Card where

import Data.Char
import Data.Tuple (swap)

data Suit = Spades
          | Hearts
          | Diamonds
          | Clubs
          deriving (Eq, Show)

-- | Returns a Char representation of a Suit.
suitToChar :: Suit -> Char
suitToChar Spades   = 'S'
suitToChar Hearts   = 'H'
suitToChar Diamonds = 'D'
suitToChar Clubs    = 'C'

-- | Returns a Suit from a Char representation.
charToSuit :: Char -> Suit
charToSuit inChar
  | upperChar == 'S' = Spades
  | upperChar == 'H' = Hearts
  | upperChar == 'D' = Diamonds
  | upperChar == 'C' = Clubs
  | otherwise        = error "Passed unexpected value to charToSuit"
  where upperChar = toUpper inChar

data Rank = Ace
          | King
          | Queen
          | Jack
          | Ten
          | Nine
          | Eight
          | Seven
          | Six
          | Five
          | Four
          | Three
          | Two
          | One
          deriving (Eq, Ord, Show)

rankList :: [(Char, Rank)]
rankList = [('a', Ace)
           ,('k', King)
           ,('q', Queen)
           ,('j', Jack)
           ,('t', Ten)
           ,('9', Nine)
           ,('8', Eight)
           ,('7', Seven)
           ,('6', Six)
           ,('5', Five)
           ,('4', Four)
           ,('3', Three)
           ,('2', Two)
           ,('1', One)
           ]

-- | Returns a Char representation of a Rank.
rankToChar :: Rank -> Maybe Char
rankToChar r = lookup r (map swap rankList)

-- | Returns a Rank from a Char representation.
charToRank :: Char -> Maybe Rank
charToRank c = lookup (toLower c) rankList
           
data Card = Card Rank Suit deriving(Show)

