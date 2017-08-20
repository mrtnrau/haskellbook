module Main where

import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Monoid (List a) where
  mempty = Nil
  mappend Nil xs         = xs
  mappend xs Nil         = xs
  mappend (Cons x xs) ys = Cons x $ mappend xs ys

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Cons f fs <*> xs = fmap f xs <> (fs <*> xs)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    pure $ Cons x (Cons y Nil)

instance Eq a => EqProp (List a) where
  (=-=) = eq

take' :: Int -> List a -> List a
take' 0 _           = Nil
take' _ Nil         = Nil
take' n (Cons x xs) = Cons x $ take' (n-1) xs

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Monoid (ZipList' a) where
  mempty = ZipList' mempty
  mappend (ZipList' xs) (ZipList' ys) = ZipList' (xs <> ys)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

repeat' :: a -> List a
repeat' x = xs
  where xs = Cons x xs

zip' :: List (a -> b) -> List a -> List b
zip' Nil _                   = Nil
zip' _ Nil                   = Nil
zip' (Cons f fs) (Cons x xs) = Cons (f x) $ zip' fs xs

instance Applicative ZipList' where
  pure a = ZipList' $ repeat' a
  (ZipList' fs) <*> (ZipList' xs) = ZipList' $ zip' fs xs

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 500 l
          ys' = let (ZipList' l) = ys
                in take' 500 l

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

trigger :: List (Bool, Bool, Bool)
trigger = undefined

trigger' :: ZipList' (Bool, Bool, Bool)
trigger' = undefined

main :: IO ()
main = do
  quickBatch $ applicative trigger
  quickBatch $ applicative trigger'
