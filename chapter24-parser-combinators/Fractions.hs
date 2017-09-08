{-# LANGUAGE OverloadedStrings #-}


module Text.Fractions where


import           Control.Applicative
import           Data.Ratio          ((%))
import           Text.Trifecta


badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"


parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "denominator can not be zero"
    _ -> return (numerator % denominator)


main :: IO ()
main = do
  let parseFraction' = parseString parseFraction mempty
  print $ parseFraction' badFraction
  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork
  print $ parseFraction' alsoBad
  -- Exercise: Unit of Success
  putStrLn "Unit of Success"
  let f = integer >>= \i -> eof >> return i
  print $ parseString f mempty "123"
  print $ parseString f mempty "123abc"



