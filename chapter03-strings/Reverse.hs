-- Reverse.hs
module Reverse where

rvrs :: String -> String
rvrs s = concat [one, two, thr]
  where one = drop 9 s
        two = take 4 $ drop 5 s
        thr = take 5 s

main :: IO ()
main = print $ rvrs "Curry is awesome"
