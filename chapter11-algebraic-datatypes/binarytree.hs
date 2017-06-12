-- binarytree.hs

module BinaryTree where

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Show, Eq, Ord)

insert :: Ord a => a -> BinaryTree a -> BinaryTree a
insert value Leaf   = Node Leaf value Leaf
insert value (Node left value' right)
  | value <= value' = Node (insert value left) value' right
  | otherwise       = Node left value' (insert value right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left value right) =
  Node (mapTree f left) (f value) (mapTree f right)

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

expectedTree :: BinaryTree Integer
expectedTree = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay =
  if mapTree (+1) testTree == expectedTree
  then print "okay"
  else error "test failed"

preorder :: BinaryTree a -> [a]
preorder = go []
  where go acc Leaf = acc
        go acc (Node left value right) = value : go (go acc right) left

inorder :: BinaryTree a -> [a]
inorder = go []
  where go acc Leaf = acc
        go acc (Node left value right) = go (value : go acc right) left

postorder :: BinaryTree a -> [a]
postorder = go []
  where go acc Leaf = acc
        go acc (Node left value right) = go (go (value : acc) right) left

testTree1 :: BinaryTree Integer
testTree1 = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree1 == [2, 1, 3]
  then print "preorder fine"
  else error "preorder not so fine"

testInorder :: IO ()
testInorder =
  if inorder testTree1 == [1, 2, 3]
  then print "inorder fine"
  else error "inorder not so fine"

testPostorder :: IO ()
testPostorder =
  if postorder testTree1 == [1, 3, 2]
  then print "postorder fine"
  else error "postorder not so fine"

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ zero Leaf = zero
foldTree f zero (Node left value right) =
  foldTree f (f value (foldTree f zero left)) right

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder
