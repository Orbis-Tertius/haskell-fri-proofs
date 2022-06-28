module Stark.MerkleTree
  ( commit
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
import Stark.Types.CapLength (CapLength (..))
import Stark.Types.Commitment (Commitment (Commitment, unCommitment))
import Stark.Types.Index (Index (Index, unIndex))
import Stark.Types.MerkleHash (MerkleHash (MerkleHash))


hashData :: Serialise a => a -> MerkleHash
hashData = MerkleHash . hash . BSL.toStrict . serialise


mergeHashes :: (MerkleHash, MerkleHash) -> MerkleHash
mergeHashes (h, k) = hashData (h, k)


commit :: Serialise a => CapLength -> BinaryTree a -> Commitment
commit capLength = commit_ capLength . fmap hashData


commit_ :: CapLength -> BinaryTree MerkleHash -> Commitment
commit_ _ (IsLeaf x) = Commitment (IsLeaf x)
commit_ capLength t@(IsNode x y) =
  let n = Tree.size t
  in if n <= unCapLength capLength
     then Commitment . IsLeaf $ commitLeaf t
     else Commitment $ IsNode
          (unCommitment (commit_ capLength x))
          (unCommitment (commit_ capLength y))


commitLeaf :: BinaryTree MerkleHash -> MerkleHash
commitLeaf (IsLeaf x) = x
commitLeaf (IsNode x y) = mergeHashes (commitLeaf x, commitLeaf y)


open_ :: CapLength -> Index -> BinaryTree MerkleHash -> AuthPath
open_ _ 0 (IsLeaf _) = AuthPath []
open_ capLength i t@(IsNode x y) =
  let n = Index . fromIntegral $ Tree.size t
      m = n `quot` 2
  in if unIndex n <= unCapLength capLength
     then AuthPath []
     else if i < m
     then (open_ capLength i x) <> AuthPath [commitLeaf y]
     else (open_ capLength (i-m) y) <> AuthPath [commitLeaf x]
open_ capLength i t = error ("open_ pattern match failure: " <> show (capLength, i, t))


open :: Serialise a => CapLength -> Index -> BinaryTree a -> AuthPath
open capLength i = open_ capLength i . fmap hashData


verify_ :: Commitment -> Index -> AuthPath -> MerkleHash -> Bool
verify_ c i p h =
  case p of
    AuthPath [] -> error "tried to verify empty path"
    AuthPath [x] ->
      (i == 0 || i == 1)
        && c == Commitment
            (mergeHashes
               (if i == 0 then (h, x)
                else if i == 1 then (x, h)
                else error "impossible case in MerkleTree.verify"))
    AuthPath (x:xs) ->
      if i `mod` 2 == 0
      then verify_ c (i `quot` 2) (AuthPath xs) (mergeHashes (h, x))
      else verify_ c (i `quot` 2) (AuthPath xs) (mergeHashes (x, h))


verify :: Serialise a => Commitment -> Index -> AuthPath -> a -> Bool
verify c i p = verify_ c i p . hashData
