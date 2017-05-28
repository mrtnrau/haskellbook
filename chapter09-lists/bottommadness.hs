-- bottommadness.hs
module BottomMadness where

-- 1)
x1 = [x^y | x <- [1..5], y <- [2, undefined]]
-- blows up

-- 2)
x2 = take 1 [x^y | x <- [1..5], y <- [2, undefined]]
-- fine

-- 3)
x3 = sum [1, undefined, 3]
-- blows up

-- 4)
x4 = length [1, 2, undefined]
-- fine

-- 5)
x5 = length $ [1, 2, 3] ++ undefined
-- blows up

-- 6)
x6 = take 1 $ filter even [1, 2, 3, undefined]
-- fine

-- 7)
x7 = take 1 $ filter even [1, 3, undefined]
-- blows up

-- 8)
x8 = take 1 $ filter odd [1, 3, undefined]
-- fine

-- 9)
x9 = take 2 $ filter odd [1, 3, undefined]
-- fine

-- 10)
x10 = take 3 $ filter odd [1, 3, undefined]
-- blows up
