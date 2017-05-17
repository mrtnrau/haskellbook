-- 1.)
lngth :: [a] -> Int
lngth l = undefined

-- 2.)
-- a) 5
-- b) 3
-- c) 2
-- d) 5

-- 3)
-- 6 / 3 works
-- 6 / length [1,2,3] does not because length
-- returns an Int which isn't not a Fractional
-- nor something more specific than a Fractional

-- 4)
-- div 6 $ length [1,2,3]

-- 5)
-- Type: Bool, Value: True

-- 6)
-- Type: Bool, Value: True

-- 7)
-- length allAwesome == 2 -- works: True
-- length [1, 'a', 3, 'b'] -- breaks: only one type allowed in lists
-- length allAwesome + length awesome -- works : 5
-- (8 == 8) && ('b' < 'a') -- works: False
-- (8 == 8) && 9 -- breaks -- can't AND a Bool and a number

-- 8)
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = reverse x == x

-- 9)
myAbs :: Integer -> Integer
myAbs x = if x < 0 then (-x) else x

-- 10)
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))

-- Correcting Syntax

-- 1)
-- x = (+)
-- f xs = x w 1
--   where w = length xs

-- 2)
-- let a = (\x -> x)

-- 3)
-- let b (x:xs) = x

-- 4)
-- f (a, b) = a

-- Match the function names to their types

-- 1) c)
-- 2) b)
-- 3) a)
-- 4) d)
