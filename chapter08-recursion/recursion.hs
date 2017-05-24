-- recursion.hs
module Recursion where

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = (f . applyTimes (n - 1) f) b

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

type Numerator = Integer
type Denominator = Integer
type Quotient = Integer

dividedBy :: Numerator -> Denominator -> Quotient
dividedBy n d
  | n < d     = 0
  | otherwise = 1 + dividedBy (n - d) d

divRemBy :: Integral a => a -> a -> (a, a)
divRemBy n d
  | n < d     = (0, n)
  | otherwise = (times + 1, remains)
    where (times, remains) = divRemBy (n - d) d

divRemBy2 :: Integral a => a -> a -> (a, a)
divRemBy2 n d = go n d 0
  where go n d c
          | n < d     = (c, n)
          | otherwise = go (n - d) d (c + 1)
