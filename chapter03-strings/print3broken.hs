-- print3broken.hs
module Print3Broken where

printSecond :: IO ()
printSecond = putStrLn greeting

main :: IO ()
main = do
  putStrLn greeting
  printSecond
--  where greeting = "Yarrrrr"

greeting :: String
greeting = "Yarrrrr"
