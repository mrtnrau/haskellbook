-- chapterexercises.hs

module ChapterExercises where

import Data.Char

-- Data.Char
-- 1)
-- isUpper :: Char -> Bool -- isLower
-- toUpper :: Char -> Char -- toLower

-- 2)
filterUpper :: String -> String
filterUpper = filter isUpper

-- 3)
capitalize :: String -> String
capitalize ""     = ""
capitalize (c:cs) = toUpper c : cs

-- 4)
capitalizeWord :: String -> String
capitalizeWord ""     = ""
capitalizeWord (c:cs) = toUpper c : capitalizeWord cs

-- 5)
firstAsCapital :: String -> Char
firstAsCapital "" = error "Empty String"
firstAsCapital s  = toUpper $ head s

-- 6)
-- firstAsCapital s = (toUpper . head) s
-- firstAsCapital = toUpper . head

-- Ciphers
ceasar :: Int -> String -> String
ceasar i = map shift
  where shift x  = chr $ base x + offset x
        offset x = mod (ord x - base x + i) 26
        base x   = if isUpper x then ord 'A' else ord 'a'

unceasar :: Int -> String -> String
unceasar i = ceasar (-i)

-- Writing your own standard functions
myAnd :: [Bool] -> Bool
myAnd []     = True
myAnd (x:xs) = x && myAnd xs

-- 1)
myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs

-- 2)
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ []     = False
myAny f (x:xs) = f x || myAny f xs

-- 3)
myElem1 :: Eq a => a -> [a] -> Bool
myElem1 _ []  = False
myElem1 e (x:xs)
  | e == x    = True
  | otherwise = myElem1 e xs

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 x = myAny (==x)

-- 4)
myReverse :: [a] -> [a]
myReverse = go []
  where go result []     = result
        go result (x:xs) = go (x : result) xs

-- 5)
squish :: [[a]] -> [a]
squish []     = []
squish (x:xs) = x ++ squish xs

-- 6)
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f

squishMapAgain :: (a -> [b]) -> [a] -> [b]
squishMapAgain _ []     = []
squishMapAgain f (x:xs) = f x ++ squishMapAgain f xs

-- 7)
squishAgain :: [[a]] -> [a]
squishAgain = squishMapAgain id

-- 8)
myOrderBy :: Ordering -> (a -> a -> Ordering) -> [a] -> a
myOrderBy _ _ []     = error "Empty List"
myOrderBy _ _ (x:[]) = x
myOrderBy o f (x:y:xs)
  | f x y == o = myOrderBy o f (x:xs)
  | otherwise  = myOrderBy o f (y:xs)

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy = myOrderBy GT

-- 9)
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy = myOrderBy LT

-- 10)
myMaximum :: (Ord a) => [a] -> a
myMaximum = myOrderBy GT compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myOrderBy LT compare
