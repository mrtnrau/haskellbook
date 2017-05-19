{-# LANGUAGE NoMonomorphismRestriction #-}

-- chapterexercises.hs

module ChapterExercises where

-- Multiple choice
-- 1) c)
-- 2) a)
-- 3) b)
-- 4) c)

-- Determine the type

--1) a) (*9) 6 -- 54 :: Num a => a
--   b) head [(0, "doge"), (1, "kitteh")] -- (0, "doge")) :: Num a => (a, [Char])
--   c) head [(0 :: Integer, "doge"), (1, "kitteh")] -- (0, "doge") :: (Integer, [Char])
--   d) if False then True else False -- False :: Bool
--   e) length [1, 2, 3, 4, 5] -- 5 :: Int
--   f) (length [1, 2, 3, 4]) > (length "TACOCAT") -- False :: Bool

-- 2) w :: Num a => a

-- 3) z :: Num a => a -> a

-- 4) f :: Fractional a => a

-- 5) f :: [Char]

-- Does it compile

-- 1) bigNum = (^) 5 $ 10 -- compiles
--    wahoo = bigNum $ 10 -- does not compile
--    -- bigNum has no more arguments it can be applied to

-- 2) does compile

-- 3) does not compile, should prop be
-- a = (+); c = a 10; d = c 200

-- 4) does not compile c unknown

-- Type variable or specific type constructor

-- fpt := fully polymorphic type
-- cpt := costrained polymorphic type
-- ct  := concrete type

-- 2) f :: zed -> Zed -> Blah
--         fpt    ct     ct

-- 3) f :: Enum b => a -> b > C
--                   fpt  cpt ct

-- 4) f :: f -> g -> C
--         fpt  fpt  ct

-- Write a type signature

-- 1)
functionH :: [a] -> a
functionH (x:_) = x

-- 2)
functionC :: (Ord a) => a -> a -> Bool
functionC x y = if x > y then True else False

-- 3)
functionS :: (a, b) -> b
functionS (x, y) = y

-- Given a type, write the function

-- 1)
i :: a -> a
i x = x

-- 2)
c :: a -> b -> a
c x y = x

-- 3)
c'' :: b -> a -> b
c'' y x = y

-- 4)
c' :: a -> b -> b
c' x y = y

-- 5)
r :: [a] -> [a]
r xs = []

-- 6)
co :: (b -> c) -> (a -> b) -> a -> c
co f g x = f $ g x

-- 7)
a :: (a -> c) -> a -> a
a f x = x

-- 8)
a' :: (a -> b) -> a -> b
a' f x = f x

-- Type-Kwon-Do

-- 1)
f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h x = g $ f x

-- 2)
data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e x = w $ q x

-- 3)
data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)

-- 4)
munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge f g x = fst $ g $ f x





