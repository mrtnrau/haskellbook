{-# LANGUAGE QuasiQuotes #-}


module TryTry where


import           Control.Applicative
import           Data.Ratio          ((%))
import           Text.RawString.QQ
import           Text.Trifecta


type FractionOrDecimal = Either Rational Integer


parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "denominator can not be zero"
    _ -> return $ numerator % denominator


parseDecimal :: Parser Integer
parseDecimal = do
  d <- decimal
  c <- anyChar
  case c of
    '\n' -> return d
    _    -> fail "Unexpected character"


parseFractionOrDecimal :: Parser FractionOrDecimal
parseFractionOrDecimal = do
  skipMany newline
  (Left <$> try parseFraction) <|> (Right <$> parseDecimal)


eitherOrSucces :: String
eitherOrSucces = [r|
1/2
1234
|]


eitherOrFail :: String
eitherOrFail = [r|
1/2
1234
1/0
|]


main :: IO ()
main = do
  let p f = parseString f mempty
  print $ p (some parseFractionOrDecimal) eitherOrSucces
  print $ p (some parseFractionOrDecimal) eitherOrFail
