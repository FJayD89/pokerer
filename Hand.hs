module Hand where

import Helpers
import Card

data Hand = HighCard Face | Pair Face  | TwoPair Face Face  | ThreeOfAKind Face  |  
     Flush Suit   |   Straight Face   |  FullHouse Face Face | FourOfAKind Face  | StraightFlush  Suit Face  | 
     RoyalFlush  Suit  deriving (Show,Eq,Ord)

data MaybeHand = MaybeHand (Maybe Hand)

instance Show (MaybeHand) where
 show (MaybeHand hand) = maybe "No Hand" show hand

mhand :: [Card] -> MaybeHand
mhand = MaybeHand . mhand'

mhand' :: [Card] -> Maybe Hand
mhand' cs
 | getRFlush cs	/= Nothing = getRFlush cs
 | getSF cs	/= Nothing = getSF cs
 | noak 4	/= Nothing = fmap FourOfAKind (noak 4)
 | getFH cs	/= Nothing = getFH cs
 | getStr cs	/= Nothing = getStr cs
 | getFlush cs	/= Nothing = getFlush cs
 | noak 3	/= Nothing = fmap ThreeOfAKind (noak 3)
 | get2P cs 	/= Nothing = get2P cs
 | noak 2 	/= Nothing = fmap Pair (noak 2)
 | otherwise		   = getHigh cs
 where noak = getNoak cs

getRFlush :: [Card] -> Maybe Hand
getRFlush cs = fmap RoyalFlush (firstMatch has_rf all_suits)
 where 
  has_rf = hasSF cs Ace
  all_suits = (fullRange :: [Suit])

getSF :: [Card] -> Maybe Hand
getSF cs = (fmap StraightFlush (fmap snd top_fs)) <*> (fmap fst top_fs)
 where 
  top_fs = firstMatch (\fs -> hasSF cs (fst fs) (snd fs)) all_fs
  all_fs = cart (reverse fullRange :: [Face]) (fullRange :: [Suit])

hasSF :: [Card] -> Face -> Suit -> Bool
hasSF = go 5
 where
  go n cs f s
   | n == 0 = True
   | f == (minBound :: Face) = False
   | otherwise = (elem card cs) && (go (n-1) cs (pred f) s )
   where card = Card f s

getNoak :: [Card] -> Int -> Maybe Face
getNoak cs n = firstMatch (hasNoak cs n) faces
 where
  faces = (reverse fullRange :: [Face])
  hasNoak cs n face = n <= (length $ filter ((face ==) . getFace) cs)

getFH :: [Card] -> Maybe Hand
getFH cs
 | Nothing == f1 = Nothing
 | otherwise = (fmap FullHouse f1) <*> (getNoak cs' 2)
 where 
  f1 = getNoak cs 3
  cs' = (filter (not . cond) cs) ++ (drop 3 (filter cond cs))
  cond = (f1 == ) . Just . getFace

getStr :: [Card] -> Maybe Hand
getStr cs = (go cs Ace)
 where 
  go cs f 
   | f == Five = Nothing
   | hasHighStraight cs f = Just (Straight f)
   | otherwise = go cs (pred f)

hasHighStraight :: [Card] -> Face -> Bool
hasHighStraight cs v = go cs v 5
 where
  go _ _ 0 = True
  go cs v n = (elem v (map getFace cs)) && (go cs (pred v) (n-1))

getFlush :: [Card] -> Maybe Hand
getFlush cs = fmap Flush (firstMatch hasFlush suits)
 where 
  suits = fullRange :: [Suit]
  hasFlush suit = 5 <= (length $ filter ((suit ==) . getSuit) cs)

get2P :: [Card] -> Maybe Hand
get2P cs
 | f1 == Nothing = Nothing
 | otherwise = (fmap TwoPair f1) <*> (getNoak cs' 2) 
 where
  cs' = (drop 2 (filter cond cs)) ++ (filter (not.cond) cs)
  f1 = getNoak cs 2
  cond = (f1 ==) . Just . getFace

getHigh :: [Card] -> Maybe Hand
getHigh [] = Nothing
getHigh cs = fmap HighCard (Just $ getFace $ last $ qsort $ cs)
