module Validation where


import           Data.Monoid
import           Test.QuickCheck          hiding (Failure, Success)
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes


data Validation e a =
    Failure e
  | Success a
  deriving (Eq, Show)


instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)


instance Monoid e => Applicative (Validation e) where
  pure = Success
  Success f <*> Success a  = Success (f a)
  Failure e <*> Failure e' = Failure (e <> e')
  _         <*> Failure e  = Failure e
  Failure e <*> _          = Failure e


instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = do
    e <- arbitrary
    a <- arbitrary
    elements [Failure e, Success a]


instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq


trigger :: Validation (String, String, String) (String, String, String)
trigger = undefined


main :: IO ()
main = quickBatch $ applicative trigger

