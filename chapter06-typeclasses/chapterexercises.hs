-- chapterexercises.hs
module ChapterExercises where

-- Multiple Choice
-- 1) c)
-- 2) b)
-- 3) a)
-- 4) c)
-- 5) a)

-- Does it typecheck?
-- 1) no Person has no instance of Show typeclass
-- 2) no Mood has no instance of Eq typeclass
-- 3) a) Blah, Woot
--    b) error expected type Mood not Num
--    c) error no instance of Ord typeclass
-- 4) yes

-- Given a datatype declaration, what can we do?
-- 1) error, we want Rocks and Yeah not String and Bool
-- 2) yes
-- 3) yes
-- 4) error, no instance of Ord for Papus

-- Match the types
-- 1) a) yes
--    b) no, we need the Num typeclass to work with numbers
-- 2) a) yes
--    b) no, we need the Fractional typeclass
-- 3) a) yes
--    b) yes
-- 4) a) yes
--    b) yes
-- 5) a) yes
--    b) yes
-- 6) a) yes
--    b) yes
-- 7) a) yes
--    b) no, can only get more specific not more general
-- 8) a) yes
--    b) same reason as 7) b)
-- 9) a) yes
--    b) yes, can get more specific
-- 10)a) yes
--    b) yes
-- 11)a) yes
--    b) no, mySort only works for [Char] not Ord a => [a]

-- Tyoe-Kwon-Do Two: Electric Typealoo

-- 1)
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f x y = (==) y $ f x

-- 2)
arith :: Num b => (a -> b) -> Integer -> a -> b
arith f i x = (fromInteger i) + (f x)
