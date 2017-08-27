module ChapterExercises where


import           Data.Foldable
import           Data.Monoid


-- 1)
newtype Constant a b = Constant a
  deriving (Eq, Show)


instance Foldable (Constant a) where
  foldMap _ _ = mempty


-- 2)
data Two a b = Two a b
  deriving (Eq, Show)


instance Foldable (Two a) where
  foldMap f (Two _ b) = f b


-- 3)
data Three a b c = Three a b c
  deriving (Eq, Show)


instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c


-- 4)
data Three' a b = Three' a b b
  deriving (Eq, Show)


instance Foldable (Three' a) where
  foldMap f (Three' _ b b') = f b <> f b'


-- 5)
data Four' a b = Four' a b b b
  deriving (Eq, Show)


instance Foldable (Four' a) where
  foldMap f (Four' _ b1 b2 b3) = f b1 <> f b2 <> f b3


-- 6)
filterF :: ( Applicative f
           , Foldable t
           , Monoid (f a)
           )
          => (a -> Bool) -> t a -> f a
filterF f = foldMap (\a -> if f a then pure a else mempty)


main :: IO ()
main = print $ getSum ((filterF even [1..10]) :: Sum Integer)
