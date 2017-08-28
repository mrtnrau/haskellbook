{-# LANGUAGE FlexibleContexts #-}


module ChapterExercises where


import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes


-- 1)
newtype Identity a = Identity { runIdentity :: a }
  deriving (Eq, Show)


instance Functor Identity where
  fmap f (Identity a) = Identity (f a)


instance Foldable Identity where
  foldMap f (Identity a) = f a


instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a


instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary


instance Eq a => EqProp (Identity a) where
  (=-=) = eq


-- 2)
newtype Constant a b = Constant { getConstant :: a }
  deriving (Eq, Show)


instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a


instance Foldable (Constant a) where
  foldMap _ _ = mempty


instance Traversable (Constant a) where
  traverse _ (Constant a) = pure $ Constant a


instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary


instance Eq a => EqProp (Constant a b) where
  (=-=) = eq


-- 3)
data Optional a =
    Nada
  | Yep a
  deriving (Eq, Show)


instance Functor Optional where
  fmap _ Nada    = Nada
  fmap f (Yep a) = Yep $ f a


instance Foldable Optional where
  foldMap f Nada    = mempty
  foldMap f (Yep a) = f a


instance Traversable Optional where
  traverse _ Nada    = pure Nada
  traverse f (Yep a) = Yep <$> f a


instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [(1, pure Nada), (3, Yep <$> arbitrary)]


instance Eq a => EqProp (Optional a) where
  (=-=) = eq


-- 4)
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)


instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)


instance Monoid (List a) where
  mempty = Nil
  mappend Nil         ys  = ys
  mappend xs          Nil = xs
  mappend (Cons x xs) ys  = Cons x (mappend xs ys)


instance Foldable List where
  foldMap _ Nil         = mempty
  foldMap f (Cons a as) = f a <> foldMap f as


instance Traversable List where
  traverse _ Nil         = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs


instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(1, pure Nil), (3, Cons <$> arbitrary <*> arbitrary)]


instance Eq a => EqProp (List a) where
  (=-=) = eq


-- 5)
data Three a b c = Three a b c
  deriving (Eq, Show)


instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)


instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c


instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c


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


-- 6)
data Pair a b = Pair a b
  deriving (Eq, Show)


instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)


instance Foldable (Pair a) where
  foldMap f (Pair a b) = f b


instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> f b


instance ( Arbitrary a
         , Arbitrary b
         )
        => Arbitrary (Pair a b) where
  arbitrary = Pair <$> arbitrary <*> arbitrary


instance ( Eq a
         , Eq b
         )
        => EqProp (Pair a b) where
  (=-=) = eq


-- 7)
data Big a b = Big a b b
  deriving (Eq, Show)


instance Functor (Big a) where
  fmap f (Big a b b') = Big a (f b) (f b')


instance Foldable (Big a) where
  foldMap f (Big a b b') = f b <> f b'


instance Traversable (Big a) where
  traverse f (Big a b b') = Big a <$> f b <*> f b'


instance ( Arbitrary a
         , Arbitrary b
         )
        => Arbitrary (Big a b) where
  arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary


instance ( Eq a
         , Eq b
         )
        => EqProp (Big a b) where
  (=-=) = eq


-- 8)
data Bigger a b = Bigger a b b b
  deriving (Eq, Show)


instance Functor (Bigger a) where
  fmap f (Bigger a b1 b2 b3) = Bigger a (f b1) (f b2) (f b3)


instance Foldable (Bigger a) where
  foldMap f (Bigger a b1 b2 b3) = f b1 <> f b2 <> f b3


instance Traversable (Bigger a) where
  traverse f (Bigger a b1 b2 b3) = Bigger a <$> f b1 <*> f b2 <*> f b3


instance ( Arbitrary a
         , Arbitrary b
         )
        => Arbitrary (Bigger a b) where
  arbitrary = Bigger
           <$> arbitrary
           <*> arbitrary
           <*> arbitrary
           <*> arbitrary

instance ( Eq a
         , Eq b
         )
        => EqProp (Bigger a b) where
  (=-=) = eq


-- 8)
data S n a = S (n a) a
  deriving (Eq, Show)


instance Functor n => Functor (S n) where
  fmap f (S na a) = S (fmap f na) (f a)


instance Foldable n => Foldable (S n) where
  foldMap f (S na a) = foldMap f na <> f a


instance Traversable n => Traversable (S n) where
  traverse f (S na a) = S <$> traverse f na <*> f a


instance ( Arbitrary (n a)
         , Arbitrary a
         )
         => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary


instance ( Eq (n a)
         , Eq a
         )
        => EqProp (S n a) where
  (=-=) = eq


-- 10)
data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)


instance Functor Tree where
  fmap _ Empty        = Empty
  fmap f (Leaf a)     = Leaf (f a)
  fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)


instance Foldable Tree where
  foldMap _ Empty        = mempty
  foldMap f (Leaf a)     = f a
  foldMap f (Node l a r) = foldMap f l <> f a <> foldMap f r


instance Traversable Tree where
  traverse f Empty        = pure Empty
  traverse f (Leaf a)     = Leaf <$> f a
  traverse f (Node l a r) = Node <$> traverse f l <*> f a <*> traverse f r


instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = frequency
    [ (1, pure Empty)
    , (1, Leaf <$> arbitrary)
    , (1, Node <$> arbitrary <*> arbitrary <*> arbitrary)
    ]


instance Eq a => EqProp (Tree a) where
  (=-=) = eq


-- Testing
type Trigger = (String, String, String)


main :: IO ()
main = do
  putStrLn "Identity"
  let trigger1 = undefined :: Identity Trigger
  quickBatch $ functor trigger1
  quickBatch $ traversable trigger1
  putStrLn "Constant"
  let trigger2 = undefined :: Constant Trigger Trigger
  quickBatch $ functor trigger2
  quickBatch $ traversable trigger2
  putStrLn "Optional"
  let trigger3 = undefined :: Optional Trigger
  quickBatch $ functor trigger3
  quickBatch $ traversable trigger3
  putStrLn "List"
  let trigger4 = undefined :: List Trigger
  quickBatch $ monoid trigger4
  quickBatch $ functor trigger4
  quickBatch $ traversable trigger4
  putStrLn "Three"
  let trigger5 = undefined :: Three Trigger Trigger Trigger
  quickBatch $ functor trigger5
  quickBatch $ traversable trigger5
  putStrLn "Pair"
  let trigger6 = undefined :: Pair Trigger Trigger
  quickBatch $ functor trigger6
  quickBatch $ traversable trigger6
  putStrLn "Big"
  let trigger7 = undefined :: Big Trigger Trigger
  quickBatch $ functor trigger7
  quickBatch $ traversable trigger7
  putStrLn "Bigger"
  let trigger8 = undefined :: Bigger Trigger Trigger
  quickBatch $ functor trigger8
  quickBatch $ traversable trigger8
  putStrLn "S"
  let trigger9 = undefined :: S [] Trigger
  quickBatch $ functor trigger9
  quickBatch $ traversable trigger9
  putStrLn "Tree"
  let trigger10 = undefined :: Tree Trigger
  quickBatch $ functor trigger10
  quickBatch $ traversable trigger10
