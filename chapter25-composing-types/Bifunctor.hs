{-# LANGUAGE InstanceSigs #-}


module Bifunctor where


import           Data.Bifunctor


-- 1)
data Deux a b = Deux a b
  deriving Show


instance Bifunctor Deux where
  bimap :: (a -> c) -> (b -> d) -> Deux a b -> Deux c d
  bimap f g (Deux a b) = Deux (f a) (g b)


-- 2)
newtype Const a b = Const a
  deriving Show


instance Bifunctor Const where
  bimap :: (a -> c) -> (b -> d) -> Const a b -> Const c d
  bimap f g (Const a) = Const (f a)


-- 3)
data Drei a b c = Drei a b c
  deriving Show


instance Bifunctor (Drei a) where
  bimap :: (b -> d) -> (c -> e) -> Drei a b c -> Drei a d e
  bimap f g (Drei a b c) = Drei a (f b) (g c)


-- 4)
data SuperDrei a b c = SuperDrei a b
  deriving Show


instance Bifunctor (SuperDrei a) where
  bimap :: (b -> d) -> (c -> e) -> SuperDrei a b c -> SuperDrei a d e
  bimap f _ (SuperDrei a b) = SuperDrei a (f b)


-- 5)
newtype SemiDrei a b c = SemiDrei a
  deriving Show


instance Bifunctor (SemiDrei a) where
  bimap :: (b -> d) -> (c -> e) -> SemiDrei a b c -> SemiDrei a d e
  bimap _ _ (SemiDrei a) = SemiDrei a


-- 6)
data Quadriceps a b c d = Quadriceps a b c d
  deriving Show


instance Bifunctor (Quadriceps a b) where
  bimap :: (c -> e) -> (d -> f) -> Quadriceps a b c d -> Quadriceps a b e f
  bimap f g (Quadriceps a b c d) = Quadriceps a b (f c) (g d)


-- 7)
data Either' a b =
    Left' a
  | Right' b
  deriving Show


instance Bifunctor Either' where
  bimap :: (a -> c) -> (b -> d) -> Either' a b -> Either' c d
  bimap f _ (Left' a)  = Left' (f a)
  bimap _ g (Right' a) = Right' (g a)
