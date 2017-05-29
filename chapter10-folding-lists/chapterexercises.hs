-- chapterexercises.hs
module ChapterExercises where

import Data.Bool

-- Warm-up and review
-- 1)
stops = "pbtdkg"
vowels = "aeiou"
-- a)
svs :: [(Char, Char, Char)]
svs = [(s1, v , s2) | s1 <- stops, v <- vowels, s2 <- stops]
-- b)
svsp :: [(Char, Char, Char)]
svsp = [(s1, v , s2) | s1 <- "p", v <- vowels, s2 <- stops]
-- c) same as above but with (nouns, verbs, nouns)

-- 2)
seekritFunc :: String -> Int
seekritFunc x =
  div (sum $ map length $ words x) (length $ words x)
-- average word length

-- Rewriting functinos using folds
myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

-- 1)
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- 2)
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False

-- 3)
myElemFold :: Eq a => a -> [a] -> Bool
myElemFold x = foldr ((||) . (==x)) False

myElemAny :: Eq a => a -> [a] -> Bool
myElemAny x = myAny (==x)

-- 4)
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- 5)
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

-- 6)
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr fold []
  where fold a b = bool b (a:b) (f a)

-- 7)
squish :: [[a]] -> [a]
squish = foldr ((++) . id) []

-- 8)
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

-- 9)
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 10)
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldl fold (head xs) xs
  where fold a b = if f a b == GT then a else b

-- 11)
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldl fold (head xs) xs
  where fold a b = if f a b == LT then a else b
