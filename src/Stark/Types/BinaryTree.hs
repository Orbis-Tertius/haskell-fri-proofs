module Stark.Types.BinaryTree ( BinaryTree (IsLeaf, IsNode) ) where


import           Codec.Serialise (Serialise)
import           GHC.Generics    (Generic)


-- IMPORTANT: This is assumed (but not guaranteed) to be a perfect
-- binary tree.
data BinaryTree a = IsLeaf a | IsNode (BinaryTree a) (BinaryTree a)
  deriving stock (Eq, Show, Functor, Generic)

instance Serialise a => Serialise (BinaryTree a)
