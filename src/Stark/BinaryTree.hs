module Stark.BinaryTree
  ( depth
  , size
  , fromList
  , (!!)
  ) where


import           Prelude                (Int, Maybe (Just, Nothing), length,
                                         quot, splitAt, (+), (-), (.), (<$>),
                                         (<), (<*>), (^))

import Stark.Cast (intToWord64)
import           Stark.Types.BinaryTree (BinaryTree (IsLeaf, IsNode))
import           Stark.Types.Index      (Index (Index, unIndex))


depth :: BinaryTree a -> Int
depth (IsLeaf _)   = 0
depth (IsNode x _) = 1 + depth x


size :: BinaryTree a -> Int
size = (2 ^) . depth


fromList :: [a] -> Maybe (BinaryTree a)
fromList [] = Nothing
fromList [x] = Just (IsLeaf x)
fromList xs =
  let (ls, rs) = splitAt (length xs `quot` 2) xs
  in IsNode <$> fromList ls <*> fromList rs


(!!) :: BinaryTree a -> Index -> Maybe a
(IsLeaf x) !! 0 = Just x
(IsLeaf _) !! _ = Nothing
(IsNode x y) !! i =
  let n = intToWord64 (size x) in
  if unIndex i < n then x !! i else y !! (i - Index n)
