-- chapterexercises.hs
module ChapterExercises where

-- exercise 2
emph :: String -> String
emph s = s ++ "!"

fifth :: String -> Char
fifth s = (!!) s 4

ninthOn :: String -> String
ninthOn s = drop 9 s

-- exercise 3
thirdLetter :: String -> Char
thirdLetter s = (!!) s 2

-- exercise 4
letterIndex :: Int -> Char
letterIndex i = (!!) "Curry is awesome!" i

-- exercise 5
rvrs :: String -> String
rvrs s = concat [one, two, thr]
  where one = drop 9 s
        two = take 4 $ drop 5 s
        thr = take 5 s
