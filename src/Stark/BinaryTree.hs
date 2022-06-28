module Stark.BinaryTree
  ( depth
  , size
  , fromList
  ) where


import Stark.Types.BinaryTree (BinaryTree (IsLeaf, IsNode))


depth :: BinaryTree a -> Integer
depth (IsLeaf _) = 0
depth (IsNode x _) = 1 + depth x


size :: BinaryTree a -> Integer
size = (2 ^) . depth


fromList :: [a] -> Maybe (BinaryTree a)
fromList [] = Nothing
fromList [x] = Just (IsLeaf x)
fromList xs =
  let (ls, rs) = splitAt (length xs `quot` 2) xs
  in IsNode <$> fromList ls <*> fromList rs
