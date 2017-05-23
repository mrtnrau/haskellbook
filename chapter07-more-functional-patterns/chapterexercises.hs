-- chapterexercises.hs
module ChapterExercises where

-- 1) d)
-- A polymorphic function may resolve to values
-- of different types, depending on inputs

-- 2) f :: Char -> String
--    g :: String -> [String]
-- b) g . f :: Char -> [String]

-- 3) f :: Ord a => a -> a -> Bool
--    applied to one argument
-- a) f' :: Ord a => a -> Bool

-- 4) f :: (a -> b) -> c
-- b) is a higher-order function

-- 5) f :: a -> a; f x = x
-- a) f True :: Boo-- 3) f :: Ord a => a -> a -> Bool
--    applied to one argument
-- a) f' :: Ord a => a -> Bool

-- 4) f :: (a -> b) -> c
-- b) is a higher-order function

-- 5) f :: a -> a;  f x = x
-- a) f True :: Booll

-- Let's write code

-- 1)
-- a)
tensDigit :: Integral a => a -> a
tensDigit = digit 10
-- b) yes
-- c)
hunsDigit :: Integral a => a -> a
hunsDigit = digit 100

digit :: Integral a => a -> a -> a
digit d = (`mod` 10) . (`div` d)

-- 2)
foldBool1 :: a -> a -> Bool -> a
foldBool1 x y b = case b of
  True  -> x
  False -> y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b
  | b         = x
  | otherwise = y

foldBool3 :: a -> a -> Bool -> a
foldBool3 x y b = if b then x else y

foldBool4 :: a -> a -> Bool -> a
foldBool4 x y True  = x
foldBool4 x y False = y

-- 3)
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)

-- 4, 5)
roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTripPF :: (Show a, Read a) => a -> a
roundTripPF = read . show

-- 6)
roundTrip2 :: (Show a, Read b) => a -> b
roundTrip2 a = read $ show a
-- i dont know how to make this work

