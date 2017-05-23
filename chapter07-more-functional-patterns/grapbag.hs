-- grabbag.hs
module GrabBag where

-- 1) all equivalent
mTh1 x y z = x * y * z
mTh2 x y   = \z -> x * y * z
mTh3 x     = \y -> \z -> x * y * z
mTh4       = \x -> \y -> \z -> x * y * z
-- mTh4 = \x y z -> x * y * z

-- 2) d)

-- 3)
-- a)
addOneIfOdd n = case odd n of
  True  -> f n
  False -> n
  where f = \n -> n + 1

-- b)
addFive = \x y -> (if x > y then y else x) + 5

-- c)
myflip :: (a -> a -> a) -> a -> a -> a
myflip f x y = f y x

