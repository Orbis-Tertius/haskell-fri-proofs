module Stark.BinaryTree
  ( depth
  , size
  ) where


import Stark.Types.BinaryTree (BinaryTree (IsLeaf, IsNode))


depth :: BinaryTree a -> Integer
depth (IsLeaf _) = 1
depth (IsNode x _) = 1 + depth x


size :: BinaryTree a -> Integer
size = (2 ^) . depth
