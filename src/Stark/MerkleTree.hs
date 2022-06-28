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
import Stark.Types.Commitment (Commitment (Commitment, unCommitment))
import Stark.Types.Index (Index (Index))
import Stark.Types.MerkleHash (MerkleHash (MerkleHash))


hashData :: Serialise a => a -> MerkleHash
hashData = MerkleHash . hash . BSL.toStrict . serialise


mergeHashes :: (MerkleHash, MerkleHash) -> MerkleHash
mergeHashes (h, k) = hashData (h, k)


commit :: Serialise a => BinaryTree a -> Commitment
commit = commit_ . fmap hashData


commit_ :: BinaryTree MerkleHash -> Commitment
commit_ (IsLeaf x) = Commitment x
commit_ (IsNode x y) =
  Commitment
  ( mergeHashes ( unCommitment (commit_ x)
                , unCommitment (commit_ y)) )


open_ :: Index -> BinaryTree MerkleHash -> AuthPath
open_ 0 (IsLeaf _) = AuthPath []
open_ i t@(IsNode x y) =
  let n = Index . fromIntegral $ Tree.size t
      m = n `quot` 2
  in if i < m
     then (open_ i x) <> AuthPath [unCommitment $ commit_ y]
     else (open_ (i-m) y) <> AuthPath [unCommitment $ commit_ x]
open_ i t = error ("open_ pattern match failure: " <> show (i, t))


open :: Serialise a => Index -> BinaryTree a -> AuthPath
open i xs = open_ i (hashData <$> xs)


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
