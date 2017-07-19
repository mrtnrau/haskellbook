module Possibly where

data Possibly a =
    LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope     = LolNope
  fmap f (Yeppers a) = Yeppers (f a)

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a)  = First a
  fmap f (Second b) = Second (f b)

-- Functor instance only for First is impossible
-- because Functor instances are only possible for Types
-- with kind * -> *.
-- a Functor instance for First would imply writing
-- instance Functor Sum where ...
-- But Sum has kind * -> * -> *
