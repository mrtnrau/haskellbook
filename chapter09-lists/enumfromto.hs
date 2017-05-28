-- enumfromto.hs
module EnumFromTo where

myEnumFromTo :: (Ord a, Enum a) => a -> a -> [a]
myEnumFromTo e1 e2
  | e1 > e2   = []
  | otherwise = go e1 e2 []
  where go e1 e2 l
          | e1 == e2  = e2 : l
          | otherwise = go e1 (pred e2) (e2 : l)

eftBool :: Bool -> Bool -> [Bool]
eftBool = myEnumFromTo

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = myEnumFromTo

eftInt :: Int -> Int -> [Int]
eftInt = myEnumFromTo

eftChar :: Char -> Char -> [Char]
eftChar = myEnumFromTo
