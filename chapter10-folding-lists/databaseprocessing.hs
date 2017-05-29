-- databaseprocessing.hs
module DatabaseProcessing where

import Data.Time

data DatabaseItem =
    DbString String
  | DbNumber Integer
  | DbDate   UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))]

-- 1)
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr fold []
  where fold (DbDate utcTime) zero = utcTime : zero
        fold _                zero = zero

-- 2)
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr fold []
  where fold (DbNumber integer) zero = integer : zero
        fold _                  zero = zero

-- 3)
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr max dawn . filterDbDate
  where dawn = UTCTime (fromGregorian 1858 11 17) (secondsToDiffTime 0)
-- -> Maybe UTCTime would probably be better

-- 4)
sumDb :: [DatabaseItem] -> Integer
sumDb = foldr fold 0
  where fold (DbNumber integer) zero = integer + zero
        fold _                  zero = zero

-- 5)
avgDb :: [DatabaseItem] -> Double
avgDb database = amount / numberof
  where numbers  = filterDbNumber database
        numberof = fromIntegral $ length numbers
        amount   = fromIntegral $ sum numbers
-- -> MayBe Double would probably be better (/0!)




