module Library where


import           Data.Foldable
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes


-- 1)
sum' :: (Foldable t, Num a) => t a -> a
sum' ta = getSum $ foldMap Sum ta


sum'' :: (Foldable t, Num a) => t a -> a
sum'' = foldr (+) 0


prop_sum :: [Int] -> Bool
prop_sum xs = sum xs == sum' xs && sum' xs == sum'' xs


-- 2)
product' :: (Foldable t, Num a) => t a -> a
product' ta = getProduct $ foldMap Product ta


product'' :: (Foldable t, Num a) => t a -> a
product'' = foldr (*) 1


prop_product :: [Int] -> Bool
prop_product xs = product xs == product' xs && product' xs == product'' xs


-- 3)
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' a ta = getAny $ foldMap (Any . (==a)) ta


elem'' :: (Foldable t, Eq a) => a -> t a -> Bool
elem'' a = foldr ((||) . (==a)) False


prop_elem :: Int -> [Int] -> Bool
prop_elem x xs = elem x xs == elem' x xs && elem' x xs == elem'' x xs


-- 4)
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr f Nothing
  where f a Nothing   = Just a
        f a (Just a') = Just $ min a a'


newtype Min a = Min {getMin :: Maybe a}
  deriving (Eq, Show)


instance Ord a => Monoid (Min a) where
  mempty = Min Nothing
  mappend m@(Min a) n@(Min b) = case (a, b) of
    (Nothing, _)     -> n
    (_, Nothing)     -> m
    (Just x, Just y) -> if x < y then m else n


instance Arbitrary a => Arbitrary (Min a) where
  arbitrary = Min <$> arbitrary


instance Eq a => EqProp (Min a) where
  (=-=) = eq


minimum'' :: (Foldable t, Ord a) => t a -> Maybe a
minimum'' ta = getMin $ foldMap (Min . Just) ta


prop_minimum :: [Int] -> Bool
prop_minimum xs = minimum' xs == minimum'' xs


-- 5)
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr f Nothing
  where f a Nothing   = Just a
        f a (Just a') = Just $ max a a'


newtype Max a = Max {getMax :: Maybe a}
  deriving (Eq, Show)


instance Ord a => Monoid (Max a) where
  mempty = Max Nothing
  mappend m@(Max a) n@(Max b) = case (a, b) of
    (Nothing, _)     -> n
    (_, Nothing)     -> m
    (Just x, Just y) -> if x > y then m else n


instance Arbitrary a => Arbitrary (Max a) where
  arbitrary = Max <$> arbitrary


instance Eq a => EqProp (Max a) where
  (=-=) = eq


maximum'' :: (Foldable t, Ord a) => t a -> Maybe a
maximum'' ta = getMax $ foldMap (Max . Just) ta


prop_maximum :: [Int] -> Bool
prop_maximum xs = maximum' xs == maximum'' xs


-- 6)
null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> False) True


prop_null :: [Int] -> Bool
prop_null xs = null xs == null' xs


-- 7)
length' :: (Foldable t) => t a -> Int
length' = foldr (\_ b -> b + 1) 0


prop_length :: [Int] -> Bool
prop_length xs = length xs == length' xs


-- 8)
toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []


prop_tolist :: [Int] -> Bool
prop_tolist xs = toList xs == toList' xs


-- 9)
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id


prop_fold :: [Sum Int] -> Bool
prop_fold xs = fold xs == fold' xs


-- 10)
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (mappend . f) mempty


main :: IO ()
main = do
  quickCheck prop_sum
  quickCheck prop_product
  quickCheck prop_elem
  quickBatch $ monoid (undefined :: Min String)
  quickCheck prop_minimum
  quickBatch $ monoid (undefined :: Max String)
  quickCheck prop_maximum
  quickCheck prop_null
  quickCheck prop_length
  quickCheck prop_tolist
  quickCheck prop_fold
