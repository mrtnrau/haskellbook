-- thyfearfulsymmetry.hs
module ThyFearfulSymmetry where

-- 1)
myWords :: String -> [String]
myWords = splitString ' '

-- 2)
firstSen  = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen  = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines = splitString '\n'

-- 3)
splitString :: Char -> String -> [String]
splitString c s = reverse $ go c s []
  where go _ "" l = l
        go c s  l = go c (lose c s) (keep c s : l)
          where lose c = dropWhile (==c) . dropWhile (/=c)
                keep c = takeWhile (/=c)


