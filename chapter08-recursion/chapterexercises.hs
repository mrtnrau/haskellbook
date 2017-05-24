-- chapterexercises.hs
module ChapterExercises where

import Data.List (intersperse)

-- Review of types
-- 1) d)
-- 2) b)
-- 3) d)
-- 4) b)

-- Reviewing currying
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "whoops"

frappe :: String -> String
frappe = flippy "haha"

-- 1) "whoops mrow woohoo!"
-- 2) "1 mrow haha"
-- 3) "whoops mrow 2 mrow haha"
-- 4) "whoops mrow blue mrow haha"
-- 5) "pink mrow haha mrow green mrow whoops mrow blue"
-- 6) "are mrow Pugs mrow awesome"

-- Recursion

-- 2)
sums :: (Eq a, Num a) => a -> a
sums n
  | n == 0   = 0
  | otherwise = n + sums (n - 1)

-- 3)
multiplies :: Integral a => a -> a -> a
multiplies x y = go x y 0
  where go x y r
          | y == 0    = r
          | otherwise = go x (y - 1) (r + x)

-- Fixing dividedBy

divByFixed :: Integral a => a -> a -> Maybe a
divByFixed x y
  | y == 0    = Nothing
  | cond      = Just $ go (abs x) (abs y) 0
  | otherwise = Just $ negate $ go (abs x) (abs y) 0
    where go x y c
            | x < y     = c
            | otherwise = go (x - y) y (c + 1)
          cond = x < 0 && y < 0 || x > 0 && y > 0

-- McCarthy 91 function

mc91 :: Integral a => a -> a
mc91 n
  | n > 100   = n - 10
  | otherwise = (mc91 . mc91) (n + 11)

-- Numbers into words

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = error "Not in [0-9]"


digits :: Int -> [Int]
digits n = go (divMod (abs n) 10) []
  where go (d, m) l
          | d == 0    = m : l
          | otherwise = go (divMod d 10) (m : l)

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits
