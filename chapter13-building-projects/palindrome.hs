module Palindrome where

import Control.Monad
import System.Exit (exitSuccess)
import Data.Char (toLower)

strip :: String -> String
strip = filter f . map toLower
  where f = (`elem` ['a'..'z'])

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  let line2 = strip line1
  case line2 == reverse line2 of
    True  -> do
      putStrLn "It's a palindrome."
      exitSuccess
    False -> putStrLn "Nope."
