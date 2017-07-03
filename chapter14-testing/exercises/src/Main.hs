{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.List (intercalate, sort)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Modifiers
import Test.Hspec

---------------------------------------------------------------------

data Trivial = Trivial deriving (Show, Eq)

instance Arbitrary Trivial where
  arbitrary = trivialGen

newtype Identity a = Identity a deriving (Show, Eq)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

data Pair a b = Pair a b deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = pairGen

data Sum a b =
    First a
  | Second b
  deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = sumGenFirstPls

data Bool' =
    False'
  | True'
  deriving (Show, Eq, Generic)

instance CoArbitrary Bool'

---------------------------------------------------------------------

trivialGen :: Gen Trivial
trivialGen = return Trivial

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

pairGen :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
pairGen = do
  a <- arbitrary
  b <- arbitrary
  return (Pair a b)

pairGenIntString :: Gen (Pair Int String)
pairGenIntString = pairGen

sumGenEqualOdds :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenEqualOdds = do
  a <- arbitrary
  b <- arbitrary
  oneof [return $ First a, return $ Second b]

sumGenFirstPls :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenFirstPls = do
  a <- arbitrary
  b <- arbitrary
  frequency [(10, return $ First a), (1, return $ Second b)]

sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGenEqualOdds

trueGen :: Gen Int
trueGen = coarbitrary True' arbitrary

falseGen :: Gen Int
falseGen = coarbitrary False' arbitrary

---------------------------------------------------------------------
-- Validating numbers into words

digitToWord :: Int -> String
digitToWord i = case i of
  0 -> "zero"
  1 -> "one"
  2 -> "two"
  3 -> "three"
  4 -> "four"
  5 -> "five"
  6 -> "six"
  7 -> "seven"
  8 -> "eight"
  9 -> "nine"
  _ -> error "Not in [0-9]"

digits :: Int -> [Int]
digits n = go (divMod (abs n) 10) []
  where go (d, m) l
          | d == 0    = m : l
          | otherwise = go (divMod d 10) (m : l)

wordNumber :: Int -> String
wordNumber = intercalate "-" . map digitToWord . digits

spec :: IO ()
spec = hspec $ do
  describe "digitToWord" $ do
    it "returns zero for 0" $
      digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $
      digitToWord 1 `shouldBe` "one"
  describe "digits" $ do
    it "returns [1] for 1" $
      digits 1 `shouldBe` [1]
    it "returns [1, 0, 0]" $
      digits 100 `shouldBe` [1, 0, 0]
  describe "wordNumber" $ do
    it "returns one-zero-zero given 100" $
      wordNumber 100 `shouldBe` "one-zero-zero"
    it "returns nine-zero-zero-one given 9001" $
      wordNumber 9001 `shouldBe` "nine-zero-zero-one"

---------------------------------------------------------------------
-- Using QuickCheck

-- 1)
half :: Fractional a => a -> a
half x = x / 2

prop_half_identity :: Double -> Bool
prop_half_identity x = ((*2) . half) x == x

-- 2)
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go y status = case status of
          (_, False)   -> status
          (Nothing, t) -> (Just y, t)
          (Just x, t)  -> (Just y, x >= y)

prop_list_ordered :: [Int] -> Bool
prop_list_ordered = listOrdered . sort

-- 3)
prop_plus_commutative :: Int -> Int -> Bool
prop_plus_commutative x y = x + y == y + x

prop_plus_associative :: Int -> Int -> Int -> Bool
prop_plus_associative x y z = x + (y + z) == (x + y) + z

-- 4)
prop_mul_commutative :: Int -> Int -> Bool
prop_mul_commutative x y = x * y == y * x

prop_mul_associative :: Int -> Int -> Int -> Bool
prop_mul_associative x y z = x * (y * z) == (x * y) * z

-- 5)
prop_quot_rem :: Int -> Int -> Property
prop_quot_rem x y = notZero y ==> quotRemIdentity x y
  where notZero = (/= 0)
        quotRemIdentity x y = quot x y * y + rem x y == x

prop_div_mod :: Int -> Int -> Property
prop_div_mod x y = notZero y ==> divModIdentity x y
  where notZero = (/= 0)
        divModIdentity x y = div x y * y + mod x y == x

-- 6)
prop_exp_commutative :: Int -> Int -> Property
prop_exp_commutative x y = positive x y ==> expCommutative x y
  where positive x y = x > 0 && y > 0
        expCommutative x y = x ^ y == y ^ x

prop_exp_associative :: Int -> Int -> Int -> Property
prop_exp_associative x y z = positive x y z ==> expAssociative x y z
  where positive x y z = x > 0 && y > 0 && z > 0
        expAssociative x y z = x ^ (y ^ z) == (x ^ y) ^ z

-- 7)
prop_reverse_identity :: [Int] -> Property
prop_reverse_identity xs =
  classify (length xs < 10)  "Short List" $
  classify (length xs >= 10) "Long List" $
  reverseId xs
  where reverseId xs = (reverse . reverse) xs == xs

-- 8)
prop_dollar :: (Int -> Int) -> Int -> Bool
prop_dollar f x = (f $ x)== f x

-- 9)
prop_folds1 :: [Int] -> [Int] -> Bool
prop_folds1 xs ys = foldr (:) ys xs == xs ++ ys

prop_folds2 :: [[Int]] -> Bool
prop_folds2 xss = foldr (++) [] xss == concat xss

-- 10)
prop_take_length :: Int -> [Int] -> Bool
prop_take_length n xs = length (take n xs) == n

-- 11)
prop_roundtrip :: Int -> Bool
prop_roundtrip x = read (show x) == x

---------------------------------------------------------------------
-- Failure
-- fails because of the finite precision of Float and Double

---------------------------------------------------------------------
-- Idempotence
twice :: (a -> a) -> a -> a
twice f = f . f

prop_sort_idempotence :: [Int] -> Bool
prop_sort_idempotence xs = sort xs == twice sort xs

---------------------------------------------------------------------
-- Generators
-- 1)
data Fool =
    Fulse
  | Frue
  deriving (Show, Eq)

instance Arbitrary Fool where
  arbitrary = genFool

genFool :: Gen Fool
genFool = elements [Fulse, Frue]

-- 2)
genFool2 :: Gen Fool
genFool2 = frequency [(2, return Fulse), (1, return Frue)]

---------------------------------------------------------------------

props :: IO ()
props = hspec $ do
  describe "prop half identity" $
    it "x * 2 / 2 is x" $
      property prop_half_identity
  describe "sort" $
    it "sorted list is ordered" $
      property prop_list_ordered
  describe "addition" $ do
    it "is commutative" $
      property prop_plus_commutative
    it "is associative" $
      property prop_plus_associative
  describe "multiplication" $ do
    it "is commutative" $
      property prop_mul_commutative
    it "is associative" $
      property prop_mul_associative
  describe "quot rem" $
    it "cancels out" $
      property prop_quot_rem
  describe "div mod" $
    it "cancels out" $
      property prop_div_mod
  -- describe "exponentiation" $ do
  --   it "is commutative" $
  --     property prop_exp_commutative
  --   it "is associative" $
  --     property prop_exp_associative

  -- hspec kills quickchecks additional classify collect output
  -- sad times
---------------------------------------------------------------------
-- main
main :: IO ()
main = do
  spec
  props
  quickCheck prop_reverse_identity
  quickCheck $ prop_dollar id
  quickCheck prop_folds1
  quickCheck prop_folds2
  quickCheck prop_take_length
  quickCheck prop_roundtrip
  quickCheck prop_sort_idempotence
