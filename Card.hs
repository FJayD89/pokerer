module Card where

import Data.Char (toUpper)

data Suit =  Spades  | Hearts  | Diamonds  | Clubs 
   deriving (Show,Eq,Ord,Enum, Bounded)

instance Read Suit where
 readsPrec _ (c:rest) = case charToSuit (toUpper c) of
  Just face -> [(face, rest)]
  Nothing   -> []
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
  readsPrec _ (c:rest) = case charToFace (toUpper c) of
    Just face -> [(face, rest)]
    Nothing   -> []
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
 readsPrec _ (f:s:rest) = case ((fmap Card (charToFace f)) <*> (charToSuit s)) of
  Just card -> [(card,rest)]
  Nothing   -> []
 readsPrec _ _ = []

cardreader = (read :: String -> Card)

card_tuple :: (Face, Suit) -> Card
card_tuple = uncurry $ Card

