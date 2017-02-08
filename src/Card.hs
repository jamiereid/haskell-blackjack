module Card where

import Data.Char

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

-- | Returns a [Char] representation of a Rank.
rankToChar :: Rank -> [Char]
rankToChar Ace   = "A"
rankToChar King  = "K"
rankToChar Queen = "Q"
rankToChar Jack  = "J"
rankToChar Ten   = "10"
rankToChar Nine  = "9"
rankToChar Eight = "8"
rankToChar Seven = "7"
rankToChar Six   = "6"
rankToChar Five  = "5"
rankToChar Four  = "4"
rankToChar Three = "3"
rankToChar Two   = "2"
rankToChar One   = "1"
           
data Card = Card Rank Suit deriving(Show)

