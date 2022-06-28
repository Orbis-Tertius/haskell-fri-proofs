{-# LANGUAGE DeriveFunctor #-}


module Stark.Types.BinaryTree ( BinaryTree (IsLeaf, IsNode) ) where


-- IMPORTANT: This is assumed (but not guaranteed) to be a perfect
-- binary tree.
data BinaryTree a = IsLeaf a | IsNode (BinaryTree a) (BinaryTree a)
  deriving (Show, Functor)
