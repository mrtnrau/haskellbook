-- applyyourself.hs
module ApplyYourself where

-- 1)
myConcat :: String -> String
myConcat x = x ++ " yo"

-- 2)
myMult :: Fractional a => a -> a
myMult x = (x / 3) * 5

-- 3)
myTake :: Int -> String
myTake x = take x "hey you"

-- 4)
myCom :: Int -> Bool
myCom x = x > length [1..10]

-- 5)
myAlph :: Char -> Bool
myAlph x = x < 'z'

