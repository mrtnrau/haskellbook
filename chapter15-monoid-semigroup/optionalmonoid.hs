module OptionalMonoid where

import           Data.Monoid

data Optional a =
    Nada
  | Only a
  deriving (Show, Eq)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend m1 m2 = case (m1, m2) of
    (Only v, Nada)     -> Only v
    (Nada, Only v)     -> Only v
    (Only v1, Only v2) -> Only (mappend v1 v2)
    (Nada, Nada)       -> Nada

