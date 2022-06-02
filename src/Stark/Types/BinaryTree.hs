module Stark.Types.BinaryTree ( BinaryTree (IsLeaf, IsNode) ) where


data BinaryTree a = IsLeaf a | IsNode (BinaryTree a) (BinaryTree a)
