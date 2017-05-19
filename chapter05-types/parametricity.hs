-- parametricity.hs
module Parametricity where

-- 1)
myId :: a -> a
myId x = x

-- 2)
f :: a -> a -> a
f x y = x
-- f x y = y

-- 3)
g :: a -> b -> b
g x y = y
