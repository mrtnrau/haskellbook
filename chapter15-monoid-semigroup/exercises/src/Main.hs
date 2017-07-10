module Main where

import           Data.Monoid     hiding ((<>))
import           Data.Semigroup
import           Test.QuickCheck hiding (Failure, Success)

-- Semigroup exercises

-- 1)
data Trivial = Trivial
  deriving (Show, Eq)

instance Semigroup Trivial where
  Trivial <> Trivial = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = a <> (b <> c) == (a <> b) <> c

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2)
newtype Identity a = Identity a
  deriving (Show, Eq)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a1) <> (Identity a2) = Identity (a1 <> a2)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = genIdentity

genIdentity :: Arbitrary a => Gen (Identity a)
genIdentity = do
  x <- arbitrary
  return $ Identity x

type IdentityAssoc =  Identity String
                   -> Identity String
                   -> Identity String
                   -> Bool

-- 3)
data Two a b = Two a b
  deriving (Show, Eq)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a1 b1) <> (Two a2 b2) = Two (a1 <> a2) (b1 <> b2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = genTwo

genTwo :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
genTwo = do
  x <- arbitrary
  y <- arbitrary
  return $ Two x y

type TwoAssoc =  Two String String
              -> Two String String
              -> Two String String
              -> Bool

-- 4)
data Three a b c = Three a b c
  deriving (Show, Eq)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a1 b1 c1) <> (Three a2 b2 c2) = Three (a1 <> a2) (b1 <> b2) (c1 <> c2)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = genThree

genThree :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
genThree = do
  x <- arbitrary
  y <- arbitrary
  z <- arbitrary
  return $ Three x y z

type ThreeAssoc =  Three String String String
                -> Three String String String
                -> Three String String String
                -> Bool

-- 5)
data Four a b c d = Four a b c d
  deriving (Show, Eq)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four a1 b1 c1 d1) <> (Four a2 b2 c2 d2) = Four (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = genFour

genFour :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
genFour = do
  w <- arbitrary
  x <- arbitrary
  y <- arbitrary
  z <- arbitrary
  return $ Four w x y z

type FourAssoc =  Four String String String String
               -> Four String String String String
               -> Four String String String String
               -> Bool

-- 6)
newtype BoolConj = BoolConj Bool
  deriving (Show, Eq)

instance Semigroup BoolConj where
  (BoolConj bool1) <> (BoolConj bool2) = BoolConj (bool1 && bool2)

instance Arbitrary BoolConj where
  arbitrary = genBoolConj

genBoolConj :: Gen BoolConj
genBoolConj = do
  b <- arbitrary
  return $ BoolConj b

type BoolConjAssoc =  BoolConj
                   -> BoolConj
                   -> BoolConj
                   -> Bool

-- 7)
newtype BoolDisj = BoolDisj Bool
  deriving (Show, Eq)

instance Semigroup BoolDisj where
  (BoolDisj bool1) <> (BoolDisj bool2) = BoolDisj (bool1 || bool2)

instance Arbitrary BoolDisj where
  arbitrary = genBoolDisj

genBoolDisj :: Gen BoolDisj
genBoolDisj = do
  b <- arbitrary
  return $ BoolDisj b

type BoolDisjAssoc =  BoolDisj
                   -> BoolDisj
                   -> BoolDisj
                   -> Bool

-- 8)
data Or a b =
    Fst a
  | Snd b
  deriving (Show, Eq)

instance Semigroup (Or a b) where
  (<>) or1 or2 = case (or1, or2) of
    (Fst a1, Fst a2) -> Fst a2
    (Fst a,  Snd b)  -> Snd b
    (Snd b,  Fst a)  -> Snd b
    (Snd b1, Snd b2) -> Snd b1

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = genOr

genOr :: (Arbitrary a, Arbitrary b) => Gen (Or a b)
genOr = do
  x <- arbitrary
  y <- arbitrary
  oneof [return $ Fst x, return $ Snd y]

type OrAssoc =  Or String String
             -> Or String String
             -> Or String String
             -> Bool

-- 9)
newtype Combine a b =
  Combine { unCombine :: a -> b }

instance Semigroup b => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine (f <> g)

-- 10)
newtype Comp a =
  Comp { unComp :: a -> a }

instance Semigroup (Comp a) where
  Comp f <> Comp g = Comp (f . g)

-- 11)
data Validation a b =
    Failure a
  | Success b
  deriving (Show, Eq)

instance Semigroup (Validation a b) where
  Failure a <> Failure _ = Failure a
  Failure a <> Success _ = Failure a
  Success _ <> Failure a = Failure a
  Success b <> Success _ = Success b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = genValidation

genValidation :: (Arbitrary a, Arbitrary b) => Gen (Validation a b)
genValidation = do
  x <- arbitrary
  y <- arbitrary
  elements [Failure x, Success y]

type ValidationAssoc =  Validation String String
                     -> Validation String String
                     -> Validation String String
                     -> Bool

-- 12)
newtype AccumulateRight a b =
  AR (Validation a b)
  deriving (Show, Eq)

instance Semigroup b => Semigroup (AccumulateRight a b) where
  AR (Failure a) <> AR (Failure _)  = AR (Failure a)
  AR (Failure a) <> AR (Success _)  = AR (Failure a)
  AR (Success _) <> AR (Failure a)  = AR (Failure a)
  AR (Success b) <> AR (Success b') = AR (Success (b <> b'))

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = genAR

genAR :: (Arbitrary a, Arbitrary b) => Gen (AccumulateRight a b)
genAR = do
  x <- arbitrary
  y <- arbitrary
  elements [AR (Failure x), AR (Success y)]

type AccumulateRightAssoc  = AccumulateRight String String
                          -> AccumulateRight String String
                          -> AccumulateRight String String
                          -> Bool

-- 13)
newtype AccumulateBoth a b =
  AB (Validation a b)
  deriving (Show, Eq)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  AB (Failure a) <> AB (Failure a') = AB (Failure (a <> a'))
  AB (Failure a) <> AB (Success _ ) = AB (Failure a)
  AB (Success _) <> AB (Failure a ) = AB (Failure a)
  AB (Success b) <> AB (Success b') = AB (Success (b <> b'))

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = genAB

genAB :: (Arbitrary a, Arbitrary b) => Gen (AccumulateBoth a b)
genAB = do
  x <- arbitrary
  y <- arbitrary
  elements [AB (Failure x), AB (Success y)]

type AccumulateBothAssoc  = AccumulateBoth String String
                         -> AccumulateBoth String String
                         -> AccumulateBoth String String
                         -> Bool

-- Monoid exercises

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity m = mappend mempty m == m

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity m = mappend m mempty == m

-- 1)
instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

type TrivialIdentity = Trivial -> Bool

-- 2)
instance (Semigroup a, Monoid a)=> Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

type IdentityIdentity = Identity String -> Bool

-- 3)
instance (Semigroup a, Semigroup b, Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

type TwoIdentity = Two String String -> Bool

-- 4)
instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

type BoolConjIdentity = BoolConj -> Bool

-- 5)
instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

type BoolDisjIdentity = BoolDisj -> Bool

-- 6)
instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
  mempty = Combine mempty
  mappend = (<>)

-- 7)
instance Monoid (Comp a) where
  mempty = Comp id
  mappend = (<>)

-- 8)
newtype Mem s a =
  Mem { runMem :: s -> (a, s) }

instance (Semigroup a, Monoid a) => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)
  mappend (Mem f) (Mem g) =
    Mem $ \s ->
      let (a, s')   = f s
          (a', s'') = g s
      in (a <> a', s'')

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
  quickCheck (semigroupAssoc :: ValidationAssoc)
  quickCheck (semigroupAssoc :: AccumulateRightAssoc)
  quickCheck (semigroupAssoc :: AccumulateBothAssoc)
  --
  quickCheck (monoidLeftIdentity :: TrivialIdentity)
  quickCheck (monoidRightIdentity :: TrivialIdentity)
  quickCheck (monoidLeftIdentity :: IdentityIdentity)
  quickCheck (monoidRightIdentity :: IdentityIdentity)
  quickCheck (monoidLeftIdentity :: TwoIdentity)
  quickCheck (monoidRightIdentity :: TwoIdentity)
  quickCheck (monoidLeftIdentity :: BoolConjIdentity)
  quickCheck (monoidRightIdentity :: BoolConjIdentity)
  quickCheck (monoidLeftIdentity :: BoolDisjIdentity)
  quickCheck (monoidRightIdentity :: BoolDisjIdentity)
