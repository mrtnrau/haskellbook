-- filtering.hs
module Filtering where

-- 1)
x1 = filter (\x -> mod x 3 == 0) [1..30]

-- 2)
x2 = length x1

-- 3)
myFilter :: String -> [String]
myFilter = filter (\x -> notElem x ["the", "a", "an"]) . words
