{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Instances
import           Test.QuickCheck
import           Test.QuickCheck.Function

-- functor laws
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) => f a -> Fun a b -> Fun b c -> Bool
functorCompose x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFI = [Int] -> Bool
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

-- 1)
type FIIdentity = Identity Int -> Bool
type FCIdentity = Identity Int -> IntToInt -> IntToInt -> Bool

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = genIdentity

genIdentity :: Arbitrary a => Gen (Identity a)
genIdentity = do
  x <- arbitrary
  return $ Identity x

-- 2)
type FIPair = Pair Int -> Bool
type FCPair = Pair Int -> IntToInt -> IntToInt -> Bool

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = genPair

genPair :: Arbitrary a => Gen (Pair a)
genPair = do
  x  <- arbitrary
  x' <- arbitrary
  return $ Pair x x'

-- 3)
type FITwo = Two String Int -> Bool
type FCTwo = Two String Int -> IntToInt -> IntToInt -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = genTwo

genTwo :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
genTwo = do
  x <- arbitrary
  y <- arbitrary
  return $ Two x y

-- 4)
type FIThree = Three String String Int -> Bool
type FCThree = Three String String Int -> IntToInt -> IntToInt -> Bool
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = genThree

genThree :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
genThree = do
  x <- arbitrary
  y <- arbitrary
  z <- arbitrary
  return $ Three x y z

-- 5)
type FIThree' = Three' String Int -> Bool
type FCThree' = Three' String Int -> IntToInt -> IntToInt -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = genThree'

genThree' :: (Arbitrary a, Arbitrary b) => Gen (Three' a b)
genThree' = do
  x  <- arbitrary
  y  <- arbitrary
  y' <- arbitrary
  return $ Three' x y y'

-- 6)
type FIFour = Four String String String Int -> Bool
type FCFour = Four String String String Int -> IntToInt -> IntToInt -> Bool

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = genFour

genFour :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
genFour = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return $ Four a b c d

-- 7)
type FIFour' = Four' String Int -> Bool
type FCFour' = Four' String Int -> IntToInt -> IntToInt -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = genFour'

genFour' :: (Arbitrary a, Arbitrary b) => Gen (Four' a b)
genFour' = do
  a1 <- arbitrary
  a2 <- arbitrary
  a3 <- arbitrary
  b  <- arbitrary
  return $ Four' a1 a2 a3 b


-- main
main :: IO ()
main = do
  quickCheck (functorIdentity :: IntFI)
  quickCheck (functorCompose :: IntFC)
  -- 1)
  quickCheck (functorIdentity :: FIIdentity)
  quickCheck (functorCompose :: FCIdentity)
  -- 2)
  quickCheck (functorIdentity :: FIPair)
  quickCheck (functorCompose :: FCPair)
  -- 3)
  quickCheck (functorIdentity :: FITwo)
  quickCheck (functorCompose :: FCTwo)
  -- 4)
  quickCheck (functorIdentity :: FIThree)
  quickCheck (functorCompose :: FCThree)
  -- 5)
  quickCheck (functorIdentity :: FIThree')
  quickCheck (functorCompose :: FCThree')
  -- 6)
  quickCheck (functorIdentity :: FIFour)
  quickCheck (functorCompose :: FCFour)
  -- 7)
  quickCheck (functorIdentity :: FIFour')
  quickCheck (functorCompose :: FCFour')


