module Card where

import Data.Char (toUpper)

data Suit =  Spades  | Hearts  | Diamonds  | Clubs 
   deriving (Show,Eq,Ord,Enum, Bounded)

instance Read Suit where
 readsPrec _ (c:rest) = maybe [] (\f -> [(f, rest)]) msuit
  where msuit = charToSuit (toUpper c)
 readsPrec _ _ = []

charToSuit :: Char -> Maybe Suit
charToSuit c = lookup (toUpper c) [ 
	 ('C', Clubs),
	 ('D', Diamonds),
	 ('H', Hearts),
	 ('S', Spades)
	]

getSuit (Card  myFace mySuit) = mySuit

 ---------

data Face = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen |  King | Ace
   deriving (Show,Eq,Ord,Enum, Bounded)

instance Read Face where
  readsPrec _ (c:rest) = maybe [] (\c -> [(c,rest)]) mface
   where mface = charToFace (toUpper c)
  readsPrec _ _ = []

charToFace :: Char -> Maybe Face
charToFace c = lookup (toUpper c) [ 
	 ('2', Two), ('3', Three), ('4', Four),	 ('5', Five),
	 ('6', Six), ('7', Seven), ('8', Eight), ('9', Nine),
         ('T', Ten), ('J', Jack),  ('Q', Queen), ('K', King), ('A', Ace)
	]

getFace (Card  myFace mySuit) = myFace

 --------

data Card = Card Face Suit deriving (Show, Eq, Ord)

instance Read Card where
 readsPrec _ (f:s:rest) = maybe [] (\c -> [(c,rest)]) mcard
  where mcard = ((fmap Card (charToFace f)) <*> (charToSuit s))
 readsPrec _ _ = []

cardreader = (read :: String -> Card)

card_tuple :: (Face, Suit) -> Card
card_tuple = uncurry $ Card

