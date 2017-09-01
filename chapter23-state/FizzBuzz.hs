module FizzBuzz where


import           Control.Monad
import           Control.Monad.Trans.State


fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Buzz"
  | n `mod` 3 == 0 = "Fizz"
  | otherwise = show n


fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = execState (mapM_ addResult list) []


addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)


fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo from to = execState (mapM_ addResult [to,to-1..from]) []


main :: IO ()
main = do
  let a1 = reverse $ fizzbuzzList [1..100]
  let a2 = fizzbuzzFromTo 1 100
  print $ a1 == a2



