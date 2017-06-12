-- dogtypes.hs

module DogTypes where

data Doggies a =
    Husky a
  | Mastiff a
  deriving (Eq, Show)

-- 1) Doggies is a type constructor
-- 2) :k Doggies -- * -> *
-- 3) :k Doggies String -- *
-- 4) :t Husky 10 -- Num a => Doggies a
-- 5) :t Hushky (10 :: Integer) -- Doggies Integer
-- 6) :t Mastiff "Scooby Doo" -- Doggies String
