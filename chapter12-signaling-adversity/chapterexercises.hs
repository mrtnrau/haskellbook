-- chapterexercises.hs

module Chapterexercises where

-- Determine the kinds
-- 1) * -> *
-- 2) a: * -> *, f: * -> *

-- String processing
-- 1)
replaceThe :: String -> String
replaceThe text = unwords $ map replace $ words text
  where replace wd = case wd of
          "the" -> "a"
          _     -> wd

-- 2)
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel text = go $ words text
  where go (x:y:xs) =
            if x == "the" && elem (head y) "aeiou"
            then 1 + go xs
            else go xs
        go _ = 0

-- 3)
countVowels :: String -> Integer
countVowels text = toInteger $ length $ filter (`elem` "aeiou") text

-- Validate the word
newtype Word' =
  Word' String
  deriving (Show, Eq)

word :: String -> Maybe Word'
word wd =
  if vowels > (toInteger (length wd) - vowels)
  then Nothing
  else Just $ Word' wd
    where vowels = countVowels wd

-- It's only Natural
data Nat =
    Zero
  | Succ Nat
  deriving (Show, Eq)

natToInteger :: Nat -> Integer
natToInteger nat = case nat of
  Zero     -> 0
  (Succ n) -> 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat int
  | int < 0   = Nothing
  | otherwise = Just $ go int
  where go 0 = Zero
        go n = Succ $ go (n-1)
