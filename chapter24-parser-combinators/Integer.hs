module Integer where


import           Control.Applicative ((<|>))
import           Data.Char           (digitToInt)
import           Text.Trifecta


parseDigit :: Parser Char
parseDigit = oneOf "0123456789" <?> "digit: [0-9]"


base10Integer :: Parser Integer
base10Integer = do
  digits <- some parseDigit
  let ints = digitToInt <$> digits
  return $ toInteger $ foldl (\b a -> b * 10 + a) 0 ints


parseSign :: Parser Char
parseSign = option '+' (char '-' <|> char '+') <?> "sign: + or - or no sign"


base10Integer' :: Parser Integer
base10Integer' = do
  sign <- parseSign
  integer <- base10Integer
  case sign of
    '+' -> return integer
    '-' -> return $ negate integer


main :: IO ()
main = do
  print $ parseString parseDigit mempty "123"
  print $ parseString parseDigit mempty "abc"
  print $ parseString base10Integer mempty "123abc"
  print $ parseString base10Integer mempty "abc"
  print $ parseString base10Integer' mempty "-123abc"
  print $ parseString base10Integer' mempty "+123abc"

