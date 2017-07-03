module Addition where

import Test.Hspec
import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d     = (count, n)
          | otherwise = go (n - d) d (count + 1)

multBySum :: (Eq a, Num a) => a -> a -> a
multBySum x y = go x y 0
  where go a b acc
          | b == 0    = acc
          | otherwise = go a (f b 1) (g acc a)
        (f, g) =
          if signum y == (-1) then
            ((+), (-))
          else
            ((-), (+))

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genTriple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genTriple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [(1, return Nothing), (3, return (Just a))]

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQC :: IO ()
runQC = quickCheck prop_additionGreater

main :: IO ()
main = hspec $ do
  describe "Addition" $
    it "x + 1 is always greater than x" $
      property $ \x -> x + 1 > (x :: Integer)
  describe "Division" $ do
    it "15 divided by 3 is 5" $
      dividedBy 15 3 `shouldBe` (5, 0 :: Integer)
    it "22 divided by 5 is 4 remainder 2" $
      dividedBy 22 5 `shouldBe` (4, 2 :: Integer)
  describe "Multiplication" $ do
    it "4 times 5 is 20" $
      multBySum 4 5 `shouldBe` (20 :: Integer)
    it "4 times -5 is -20" $
      multBySum 4 (-5) `shouldBe` ((-20) :: Integer)
    it "-4 times 5 is -20" $
      multBySum (-4) 5 `shouldBe` ((-20) :: Integer)
    it "-4 times -5 is 20" $
      multBySum (-4) (-5) `shouldBe` (20 :: Integer)

