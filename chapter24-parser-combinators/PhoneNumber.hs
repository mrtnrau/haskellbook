module PhoneNumber where


import           Control.Applicative ((<|>))
import           Text.Trifecta


type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int


data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber
  deriving (Eq, Show)


npa :: Parser Int
npa = read <$> count 3 digit


exc :: Parser Int
exc = read <$> count 3 digit


lin :: Parser Int
lin = read <$> count 4 digit


-- exercise is more complicated than it looks
--
-- z := country prefix
-- a := NumberingPlanArea
-- b := Exchange
-- c := LineNumber
--
-- there are the following formats:
-- abc      (1)
--   a-b-c  (2)
-- z-a-b-c  (3)
-- (a) b-c  (4)
--
-- i assume that one is not allowed to mix formats:
-- e.g. z-(a) bc  would not be allowed
--
-- i did split up format (2) (3) and (4) into:
--   head: optional z- and a- or (a)
--   tail: b-c


parseFormat1 :: Parser PhoneNumber
parseFormat1 = PhoneNumber <$> npa <*> exc <*> lin


parseFormat234 :: Parser PhoneNumber
parseFormat234 = do
  a <- choice [try parseFormat234Head1, parseFormat234Head2]
  (b, c) <- parseFormat234Tail
  return $ PhoneNumber a b c


parseFormat234Head1 :: Parser NumberingPlanArea
parseFormat234Head1 = do
  optional $ try (digit >> char '-')
  a <- npa
  char '-'
  return a


parseFormat234Head2 :: Parser NumberingPlanArea
parseFormat234Head2 = do
  char '('
  a <- npa
  char ')'
  char ' '
  return a


parseFormat234Tail :: Parser (Exchange, LineNumber)
parseFormat234Tail = do
  b <- exc
  char '-'
  c <- lin
  return (b, c)


parsePhone :: Parser PhoneNumber
parsePhone = choice [try parseFormat1, parseFormat234]


main :: IO ()
main = do
  let p = parseString parsePhone mempty
  print $ p "123-456-7890"
  print $ p "1234567890"
  print $ p "(123) 456-7890"
  print $ p "1-123-456-7890"
