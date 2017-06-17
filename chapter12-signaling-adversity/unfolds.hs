-- unfolds.hs

module Unfolds where

-- 1)
myIterate :: (a -> a) -> a -> [a]
myIterate f start = start : myIterate f (f start)

-- 2)
myUnfoldr :: (b -> Maybe (a,b)) -> b -> [a]
myUnfoldr f start = case f start of
  Just (start, next) -> start : myUnfoldr f next
  Nothing            -> []

-- 3)
betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just (x, f x))

-- BinaryTree

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Show, Eq)

-- 1)
unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f seed = case f seed of
  Just (left, value, right) -> Node (unfold f left) value (unfold f right)
  Nothing -> Leaf

-- 2)
treeBuild :: Integer -> BinaryTree Integer
treeBuild = unfold f
  where f 0 = Nothing
        f x = Just (x-1, x, x-1)
