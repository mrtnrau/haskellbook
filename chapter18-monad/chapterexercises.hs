module ChapterExercises where


import           Control.Applicative      (liftA2)
import           Control.Monad            (join, liftM2)
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes


-- 1)
data Nope a = NopeDotJpg
  deriving (Eq, Show)


instance Functor Nope where
  fmap _ _ = NopeDotJpg


instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg


instance Monad Nope where
  return = pure
  _ >>= _ = NopeDotJpg


instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg


instance EqProp (Nope a) where
  (=-=) = eq


-- 2)
data PhhhbbtttEither b a =
    Left' a
  | Right' b
  deriving (Eq, Show)


instance Functor (PhhhbbtttEither b) where
  fmap f (Left' a)  = Left' (f a)
  fmap _ (Right' b) = Right' b


instance Applicative (PhhhbbtttEither b) where
  pure = Left'
  Right' b <*> _        = Right' b
  _        <*> Right' b = Right' b
  Left' f  <*> Left' a  = Left' (f a)


instance Monad (PhhhbbtttEither b) where
  return = pure
  Right' b >>= _ = Right' b
  Left' a  >>= f = f a


instance ( Arbitrary a
         , Arbitrary b
         )
        => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = oneof [Left' <$> arbitrary, Right' <$> arbitrary]


instance ( Eq a
         , Eq b
         )
        => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq


-- 3)
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)


instance Functor Identity where
  fmap f (Identity a) = Identity (f a)


instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)


instance Monad Identity where
  return = pure
  Identity a >>= f = f a


instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary


instance Eq a => EqProp (Identity a) where
  (=-=) = eq


-- 4)
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)


instance Monoid (List a) where
  mempty = Nil
  mappend Nil         list = list
  mappend list        Nil  = list
  mappend (Cons a as) list = Cons a $ mappend as list


instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)


instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _        = Nil
  _   <*> Nil      = Nil
  Cons f fs <*> as = fmap f as <> (fs <*> as)


instance Monad List where
  return = pure
  Nil       >>= _ = Nil
  Cons a as >>= f = f a <> (as >>= f)


instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(1, pure Nil), (2, Cons <$> arbitrary <*> arbitrary)]


instance Eq a => EqProp (List a) where
  (=-=) = eq


-- 1)
j :: Monad m => m (m a) -> m a
j = join
-- or j x = x >>= id


-- 2)
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap


-- 3)
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2
-- or l2 = liftA2


-- 4)
a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)


-- 5)
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh []     _ = pure []
meh (x:xs) f = (:) <$> f x <*> meh xs f


-- 6)
flipType :: Monad m => [m a] -> m [a]
flipType xs = meh xs id


-- Testing
type S = String


main :: IO ()
main = do
  -- 1)
  let trigger1 = undefined :: Nope (S, S, S)
  quickBatch $ functor trigger1
  quickBatch $ applicative trigger1
  quickBatch $ monad trigger1
  -- 2)
  let trigger2 = undefined :: PhhhbbtttEither S (S, S, S)
  quickBatch $ functor trigger2
  quickBatch $ applicative trigger2
  quickBatch $ monad trigger2
  -- 3)
  let trigger3 = undefined :: Identity (S, S, S)
  quickBatch $ functor trigger3
  quickBatch $ applicative trigger3
  quickBatch $ monad trigger3
  -- 4)
  let trigger4 = undefined :: List (S, S, S)
  quickBatch $ monoid (undefined :: List S)
  quickBatch $ functor trigger4
  quickBatch $ applicative trigger4
  quickBatch $ monad trigger4

