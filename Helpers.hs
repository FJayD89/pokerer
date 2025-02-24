module Helpers where

cart :: [a] -> [b] -> [(a,b)]
cart [] ys = []
cart (x:xs) (ys) = (map (x,) ys) ++ (cart xs ys)

fullRange :: (Bounded a, Enum a) => [a]
fullRange = enumFromTo minBound maxBound

empty :: [a] -> Bool
empty [] = True
empty xs = False

firstMatch :: (a -> Bool) -> [a] -> Maybe a
firstMatch cond xs
 | empty xs = Nothing
 | cond $ head xs = Just $ head xs
 | otherwise = firstMatch cond $ tail xs

-- sort a list of any Ord type
qsort ::  Ord a =>  [a] -> [a]
qsort  []            =            []
qsort  (x:xs)        =   (qsort (filter ( < x) xs))  ++ (x : (qsort [y | y <- xs, y > x]) )


