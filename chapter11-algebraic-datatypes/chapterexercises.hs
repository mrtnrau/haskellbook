-- chapterexercises.hs

module ChapterExercises where

import Data.Char

-- 1) a) Weekday is a type with 5 data constructors
data Weekday =
    Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  deriving (Show, Eq)

-- 2) c) and partial function ... :set -Wall
f :: Weekday -> String
f Friday = "Miller Time"

-- 3) b) must begin with a capital letter

-- 4) c) delivers the final element of xs and throws an error for []
g xs = xs !! (length xs - 1)

-- Ciphers
shift :: Char -> Char -> Char
shift sh ch = chr $ base ch + offset ch sh
  where base ch = if isUpper ch then ord 'A' else ord 'a'
        offset ch sh = mod (ord ch - base ch + ord sh - base sh) 26

vigenere :: String -> String -> String
vigenere code text = go code text
  where go _      []       = []
        go []     text     = go code text
        go code   (' ':ts) = ' ' : go code ts
        go (c:cs) (t:ts)   = shift c t : go cs ts

-- As Patterns
-- 1)
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf = undefined

-- 2)
capitalizeWords :: String -> [(String, String)]
capitalizeWords text = map (\word@(c:cs) -> (word, toUpper c : cs)) $ words text

-- Language Exercises
-- 1)
capitalizeWord :: String -> String
capitalizeWord []     = []
capitalizeWord (c:cs) = toUpper c : cs

-- 2)
capitalizeParagraph :: String -> String
capitalizeParagraph text = unwords $ map capitalizeWord $ splitOn '.' text

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn char text = (paragraph ++ "."): splitOn '.' (drop 2 rest)
  where paragraph = takeWhile (/=char) text
        rest      = dropWhile (/=char) text

-- Hutton's Razor
data Expr =
    Lit Integer
  | Add Expr Expr

-- 1)
eval :: Expr -> Integer
eval (Lit int)   = int
eval (Add e1 e2) = eval e1 + eval e2

expr :: Expr
expr = Add (Lit 1) (Lit 2)

-- 2)
printExpr :: Expr -> String
printExpr (Lit int)   = show int
printExpr (Add e1 e2) = printExpr e1 ++ " + " ++ printExpr e2
