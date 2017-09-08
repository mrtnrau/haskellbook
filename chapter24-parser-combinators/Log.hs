{-# LANGUAGE QuasiQuotes #-}


module Log where


import           Control.Applicative ((<|>))
import           Data.Char           (isSpace)
import           Text.Printf
import           Text.RawString.QQ
import           Text.Trifecta


type Year = Int
type Month = Int
type Day = Int
type Hours = Int
type Minutes = Int
type Description = String


data Date = Date Year Month Day


instance Show Date where
  show (Date year month day) =
    printf "%04d" year  ++ "-" ++
    printf "%02d" month ++ "-" ++
    printf "%02d" day


data Time = Time Hours Minutes


instance Show Time where
  show (Time hours minutes) =
    printf "%02d" hours ++ ":" ++ printf "%02d" minutes


data Activity = Activity Time Description


instance Show Activity where
  show (Activity time desc) = show time ++ " " ++ desc


data Entry = Entry Date [Activity]


instance Show Entry where
  show (Entry date activities) =
    "\n# " ++ show date ++ "\n" ++ prettyList activities


newtype Log = Log [Entry]


instance Show Log where
  show (Log entries) = prettyList entries


prettyList :: Show a => [a] -> String
prettyList []     = ""
prettyList (x:xs) = show x ++ "\n" ++ prettyList xs


parseComment :: Parser String
parseComment = do
  string "--"
  manyTill anyChar (try $ char '\n')


parseDate :: Parser Date
parseDate = do
  string "# "
  year <- count 4 digit
  char '-'
  month <- count 2 digit
  char '-'
  day <- count 2 digit
  return $ Date (read year) (read month) (read day)


parseTime :: Parser Time
parseTime = do
  hours <- count 2 digit
  char ':'
  minutes <- count 2 digit
  return $ Time (read hours) (read minutes)


parseDescription :: Parser Description
parseDescription = manyTill anyChar (try (parseComment <|> string "\n"))


parseActivity :: Parser Activity
parseActivity = do
  time <- token parseTime
  description <- token parseDescription
  many $ token parseComment
  return $ Activity time (trimRight description)


trimRight :: String -> String
trimRight str      | all isSpace str = ""
trimRight (c : cs) = c : trimRight cs


parseEntry :: Parser Entry
parseEntry = do
  many $ token parseComment
  date <- token parseDate
  many $ token parseComment
  activities <- some (token parseActivity)
  return $ Entry date activities


parseLog :: Parser Log
parseLog = do
  whiteSpace
  entries <- some (token parseEntry)
  whiteSpace
  return $ Log entries


-- assuming that every entry has at least two activites
timeSpent :: Entry -> Int
timeSpent (Entry _ activities) =
  timeToMinutes (times !! (length times - 1)) -
  timeToMinutes (times !! 0)
  where times = fmap (\(Activity time _) -> time) activities


avgTimeSpent :: Entry -> Double
avgTimeSpent entry = time / count
  where time = fromIntegral $ timeSpent entry
        count = fromIntegral $ countActivities entry


countActivities :: Entry -> Int
countActivities (Entry _ activities) = length activities - 1


timeToMinutes :: Time -> Int
timeToMinutes (Time hours minutes) = hours * 60 + minutes


fromLog :: Result Log -> Maybe [Entry]
fromLog (Success (Log entries)) = Just entries
fromLog (Failure _)             = Nothing



example1 :: String
example1 = [r|

-- wee comment


# 2025-02-05 -- more comments
08:00 Breakfast  -- commentssss
-- comment mania
09:00 Sanitizing moisture collector -- comment raptor
-- commentopia
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep
-- final comment
-- its the final comments

|]


example2 :: String
example2 = [r|
-- comment
# 2025-02-07
08:00 Breakfast
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]


main :: IO ()
main = do
  let p i = parseString i mempty
      ex1 = p parseLog example1
      ex2 = p parseLog example2
      ex12 = p parseLog (example1 ++ example2)
  print ex1
  print ex2
  print ex12
  print $ (fmap . fmap) timeSpent (fromLog ex1)
  print $ (fmap . fmap) timeSpent (fromLog ex1)
  print $ (fmap . fmap) timeSpent (fromLog ex12)
  print $ (fmap . fmap) avgTimeSpent (fromLog ex1)
  print $ (fmap . fmap) avgTimeSpent (fromLog ex2)
  print $ (fmap . fmap) avgTimeSpent (fromLog ex12)
