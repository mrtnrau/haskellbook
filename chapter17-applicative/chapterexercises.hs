module ChapterExercises where


import           Control.Applicative      (liftA3)
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes


-- 1)
-- []
-- pure :: a -> [] a
-- (<*>) :: [] (a -> b) -> [] a -> [] b


-- 2)
-- IO
-- pure :: a -> IO a
-- (<*>) :: IO (a -> b) -> IO a -> IO b


-- 3)
-- (,) a
-- pure :: b -> (,) a b
-- (<*>) :: (,) a (b -> c) -> (,) a b -> (,) a c


-- 4)
-- (->) e
-- pure :: a -> (->) e a
-- (<*>) :: (->) e (a -> b) -> (->) e a -> (->) e b


-- 1)
data Pair a = Pair a a
  deriving (Eq, Show)


instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')


instance Applicative Pair where
  pure a = Pair a a
  Pair f g <*> Pair a a' = Pair (f a) (g a')


instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary


instance Eq a => EqProp (Pair a) where
  (=-=) = eq


-- 2)
data Two a b = Two a b
  deriving (Eq, Show)


instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)


instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  Two a f <*> Two a' b = Two (a <> a') (f b)


instance ( Arbitrary a
         , Arbitrary b
         )
        => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary


instance ( Eq a
         , Eq b
         )
        => EqProp (Two a b) where
  (=-=) = eq


-- 3)
data Three a b c = Three a b c
  deriving (Eq, Show)


instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)


instance ( Monoid a
         , Monoid b
         )
        => Applicative (Three a b) where
  pure = Three mempty mempty
  Three a b f <*> Three a' b' c = Three (a <> a') (b <> b') (f c)


instance ( Arbitrary a
         , Arbitrary b
         , Arbitrary c
         )
        => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary


instance ( Eq a
         , Eq b
         , Eq c
         )
        => EqProp (Three a b c) where
  (=-=) = eq


-- 4)
data Three' a b = Three' a b b
  deriving (Eq, Show)


instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')


instance Monoid a => Applicative (Three' a) where
  pure b = Three' mempty b b
  Three' a f g <*> Three' a' b b' = Three' (a <> a') (f b) (g b')


instance ( Arbitrary a
         , Arbitrary b
         )
        => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary


instance ( Eq a
         , Eq b
         )
        => EqProp (Three' a b) where
  (=-=) = eq


-- 5)
data Four a b c d = Four a b c d
  deriving (Eq, Show)


instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)


instance ( Monoid a
         , Monoid b
         , Monoid c
         )
        => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  Four a b c f <*> Four a' b' c' d =
    Four (a <> a') (b <> b') (c <> c') (f d)


instance (  Arbitrary a
         ,  Arbitrary b
         ,  Arbitrary c
         ,  Arbitrary d
         )
         => Arbitrary (Four a b c d) where
  arbitrary =
    Four <$> arbitrary
         <*> arbitrary
         <*> arbitrary
         <*> arbitrary


instance ( Eq a
         , Eq b
         , Eq c
         , Eq d
         )
        => EqProp (Four a b c d) where
  (=-=) = eq


-- 6)
data Four' a b = Four' a a a b
  deriving (Eq, Show)


instance Functor (Four' a) where
  fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)


instance Monoid a => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  Four' a1 a2 a3 f <*> Four' a1' a2' a3' b =
    Four' (a1 <> a1') (a2 <> a2')(a3 <> a3') (f b)


instance ( Arbitrary a
         , Arbitrary b
         )
        => Arbitrary (Four' a b) where
  arbitrary =
    Four' <$> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary


instance ( Eq a
         , Eq b
         )
        => EqProp (Four' a b) where
  (=-=) = eq


-- Combinations
stops :: String
stops = "pbtdkg"


vowels :: String
vowels = "aeiou"


combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)


type S = String


trigger1 :: Pair (S, S, S)
trigger1 = undefined


trigger2 :: Two S (S, S, S)
trigger2 = undefined


trigger3 :: Three S S (S, S, S)
trigger3 = undefined


trigger4 :: Three' S (S, S, S)
trigger4 = undefined

trigger5 :: Four S S S (S, S, S)
trigger5 = undefined


trigger6 :: Four' S (S, S, S)
trigger6 = undefined


main :: IO ()
main = do
  -- 1)
  quickBatch $ applicative trigger1
  -- 2)
  quickBatch $ applicative trigger2
  -- 3)
  quickBatch $ applicative trigger3
  -- 4)
  quickBatch $ applicative trigger4
  -- 5)
  quickBatch $ applicative trigger5
  -- 6)
  quickBatch $ applicative trigger6
