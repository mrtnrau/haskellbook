-- goats.hs

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Goats where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany int = int > 42

instance TooMany GoatsNames where
  tooMany (GoatsNames (int, string)) = tooMany int

instance TooMany (Int, Int) where
  tooMany (int1, int2) = tooMany (int1 + int2)
-- can do that with second language pragma

newtype Goats =
  Goats Int
  deriving (Show, Eq, TooMany)
-- can do that with first language pragma

newtype GoatsNames =
  GoatsNames (Int, String)
  deriving (Show, Eq)
