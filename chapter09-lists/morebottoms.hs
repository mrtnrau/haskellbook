-- morebottoms.hs
module MoreBottoms where

import Data.Bool

-- 1)
x1 = take 1 $ map (+1) [undefined, 2, 3]
-- blows up

-- 2)
x2 = take 1 $ map (+1) [1, undefined, 3]
-- fine -- [2]

-- 3)
x3 = take 2 $ map (+1) [1, undefined, 3]
-- blows up

-- 4)
itIsMystery :: String -> [Bool]
itIsMystery = map (\x -> elem x "aeiou")
-- get indexes of the string that are vocals as True

-- 5)
-- a) map (^2) [1..10] -- [1,4,9,16,25,36,49,64,91,100]
-- b) map minimum [[1..10], [10..20], [20..30]] -- [1,10,20]
-- c) map sum [[1..5], [1..5], [1..5]] -- [15,15,15]

-- 6)
-- map (\x -> if x == 3 then (-x) else (x)) [1..10]
f = map (\x -> bool x (-3) (x==3)) [1..10]
