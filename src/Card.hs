module Card where

import Data.Char
import Data.Tuple (swap)

data Suit = Spades
          | Hearts
          | Diamonds
          | Clubs
          deriving (Eq, Show)

suitList :: [(Char, Suit)]
suitList = [('s', Spades)
           ,('h', Hearts)
           ,('d', Diamonds)
           ,('c', Clubs)
           ]

-- | Returns a Char representation of a Suit.
suitToChar :: Suit -> Maybe Char
suitToChar s = lookup s (map swap suitList)

-- | Returns a Suit from a Char representation.
charToSuit :: Char -> Maybe Suit
charToSuit c = lookup (toLower c) suitList

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

-- | Returns a [Char] representation of a Card.
showCard :: Card -> [Maybe Char]
showCard (Card r s) = [rank, suit]
  where rank = rankToChar r
        suit = suitToChar s

-- | Returns a Card from a [Char] card representation
-- readCard :: [Char] -> Card
-- readCard [r,s] = (Card rank suit) -- @Broken rank and suit are wrong type
--   where rank = charToRank r
--         suit = charToSuit s
