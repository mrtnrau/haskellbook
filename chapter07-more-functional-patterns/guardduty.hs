-- guardduty.hs
module GuardDuty where

-- 1, 2)
avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | otherwise = 'F' -- should go last
  | y >= 0.9  = 'A'
  | y >= 0.8  = 'B'
  | y >= 0.7  = 'C' -- should be third
  | y >= 0.6  = 'D'
  where y = x / 100

-- 3)
pal :: Eq a => [a] -> Bool
pal xs
  | xs == reverse xs = True
  | otherwise        = False
-- b) True when xs is a palindrome

-- 4) [a] with Eq Typeclass instance of a
-- 5) look at type declaration
-- 6)
numbers :: (Num b, Num a, Ord a) => a -> b
numbers x
  | x < 0  = -1
  | x == 0 = 0
  | x > 0  = 1
-- c) an indication of wheather its argument
--    is a positive or negative number or zero

-- 7) Num a, Ord a
-- 8) look at type declaration
