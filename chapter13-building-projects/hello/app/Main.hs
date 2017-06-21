module Main where

import Hello
import Dogs
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Please enter your name: "
  name <- getLine
  hello name
  dogs
