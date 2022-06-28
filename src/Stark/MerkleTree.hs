module Stark.MerkleTree
  ( dataToLeaf
  , commit
  , commit_
  , open_
  , open
  , verify_
  , verify
  ) where


import Codec.Serialise (Serialise, serialise)
import qualified Data.ByteString.Lazy as BSL

import qualified Stark.BinaryTree as Tree
import Stark.Hash (hash)
import Stark.Types.AuthPath (AuthPath (AuthPath))
import Stark.Types.BinaryTree (BinaryTree (IsLeaf, IsNode))
import Stark.Types.Commitment (Commitment (Commitment, unCommitment))
import Stark.Types.Index (Index (Index))
import Stark.Types.Leaf (Leaf (Leaf, unLeaf))
import Stark.Types.MerkleHash (MerkleHash (MerkleHash))


hashData :: Serialise a => a -> MerkleHash
hashData = MerkleHash . hash . BSL.toStrict . serialise


dataToLeaf :: Serialise a => a -> Leaf
dataToLeaf = Leaf . hashData


commit :: Serialise a => BinaryTree a -> Commitment
commit = commit_ . fmap hashData


commit_ :: BinaryTree MerkleHash -> Commitment
commit_ (IsLeaf x) = Commitment x
commit_ (IsNode x y) = Commitment (hashData (commit x, commit y))


open_ :: Index -> BinaryTree MerkleHash -> AuthPath
open_ 0 (IsNode (IsLeaf _) (IsLeaf x)) = AuthPath [x]
open_ 1 (IsNode (IsLeaf x) (IsLeaf _)) = AuthPath [x]
open_ 0 (IsLeaf _) = AuthPath []
open_ i t@(IsNode x y) =
  let n = Index . fromIntegral $ Tree.size t
      m = n `quot` 2
  in if i < m
     then (open_ i x) <> AuthPath [unCommitment $ commit_ y]
     else (open_ (i-m) x) <> AuthPath [unCommitment $ commit_ y]
open_ i t = error ("open_ pattern match failure: " <> show (i, t))


open :: Serialise a => Index -> BinaryTree a -> AuthPath
open i xs = open_ i (hashData <$> xs)


verify_ :: Commitment -> Index -> AuthPath -> Leaf -> Bool
verify_ c i p l =
  case p of
    AuthPath [] -> error "tried to verify empty path"
    AuthPath [x] -> c == Commitment (hashData (x, unLeaf l))
    AuthPath (x:xs) ->
      if i `mod` 2 == 0
      then verify_ c (i `quot` 2) (AuthPath xs) (dataToLeaf (unLeaf l, x))
      else verify_ c (i `quot` 2) (AuthPath xs) (dataToLeaf (x, unLeaf l))


verify :: Serialise a => Commitment -> Index -> AuthPath -> a -> Bool
verify c i p = verify_ c i p . dataToLeaf
