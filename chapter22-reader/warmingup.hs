module WarmingUp where


import           Data.Char


cap :: [Char] -> [Char]
cap = map toUpper


rev :: [Char] -> [Char]
rev = reverse


composed :: [Char] -> [Char]
composed = rev . cap


fmapped :: [Char] -> [Char]
fmapped = fmap rev cap


tupled1 :: [Char] -> ([Char], [Char])
tupled1 = (,) <$> rev <*> cap


tupled2 :: [Char] -> ([Char], [Char])
tupled2 = do
  a <- rev
  b <- cap
  return (a, b)


tupled3 :: [Char] -> ([Char], [Char])
tupled3 = rev >>= \x -> cap >>= \y -> return (x,y)
