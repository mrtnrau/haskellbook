-- eqinstances.hs
module EqInstances where

-- 1)
data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn i) (TisAn i') = i == i'

-- 2)
data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two i1 i2) (Two i1' i2') =
    i1 == i1' && i2 == i2'

-- 3)
data StringOrInt =
    TisAnInt   Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt i)   (TisAnInt i')   = i == i'
  (==) (TisAString s) (TisAString s') = s == s'
  (==) _ _                            = False

-- 4)
data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair x1 x2) (Pair x1' x2') =
    x1 == x1' && x2 == x2'

-- 5)
data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple x y) (Tuple x' y') =
    x == x' && y == y'

-- 6)
data Which a =
    ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne x) (ThisOne x') = x == x'
  (==) (ThatOne x) (ThatOne x') = x == x'
  (==) _ _                      = False

-- 7)
data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x)   (Hello x')   = x == x'
  (==) (Goodbye y) (Goodbye y') = y == y'
  (==) _ _                      = False

