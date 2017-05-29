-- scansexercises.hs
module ScansExercises where

fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

fibsN :: Int -> Integer
fibsN = (!!) fibs

-- 1)
fibs20 :: [Integer]
fibs20 = take 20 fibs

-- 2)
fibsLessThan100 :: [Integer]
fibsLessThan100 = filter (<100) fibs20

-- 3)
factorial :: [Integer]
factorial = scanl (*) 1 [1..]

