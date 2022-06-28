{-# LANGUAGE TupleSections #-}


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
import Data.Maybe (fromMaybe)

import qualified Stark.BinaryTree as Tree
import Stark.Hash (hash)
import Stark.Types.AuthPath (AuthPath (AuthPath))
import Stark.Types.BinaryTree (BinaryTree (IsLeaf, IsNode))
import Stark.Types.CapCommitment (CapCommitment (..))
import Stark.Types.CapLength (CapLength (..))
import Stark.Types.Commitment (Commitment (Commitment))
import Stark.Types.Index (Index (Index, unIndex))
import Stark.Types.MerkleHash (MerkleHash (MerkleHash))


hashData :: Serialise a => a -> MerkleHash
hashData = MerkleHash . hash . BSL.toStrict . serialise


mergeCommitments :: (Commitment, Commitment) -> Commitment
mergeCommitments (Commitment h, Commitment k) = Commitment (hashData (h, k))


commit :: Serialise a => CapLength -> BinaryTree a -> CapCommitment
commit capLength = commit_ capLength . fmap hashData


commit_ :: CapLength -> BinaryTree MerkleHash -> CapCommitment
commit_ _ (IsLeaf x) = CapCommitment (IsLeaf (Commitment x))
commit_ capLength t@(IsNode x y) =
  let n = Tree.size t
  in if n <= unCapLength capLength
     then CapCommitment . IsLeaf $ commitCapLeaf t
     else CapCommitment $ IsNode
          (unCapCommitment (commit_ capLength x))
          (unCapCommitment (commit_ capLength y))


commitCapLeaf :: BinaryTree MerkleHash -> Commitment
commitCapLeaf (IsLeaf x) = Commitment x
commitCapLeaf (IsNode x y) = mergeCommitments (commitCapLeaf x, commitCapLeaf y)


-- wrong: skip down to cap and add paths down from there
open_ :: CapLength -> Index -> BinaryTree MerkleHash -> AuthPath
open_ _ 0 (IsLeaf _) = AuthPath []
open_ capLength i t@(IsNode x y) =
  let n = Index $ Tree.size t
      m = n `quot` 2
  in if unIndex n <= unCapLength capLength
     then (if i < m
           then (open_ capLength i x) <> AuthPath [commitCapLeaf y]
           else (open_ capLength (i-m) y) <> AuthPath [commitCapLeaf x])
     else open_ capLength (if i < m then i else i-m) (if i < m then x else y)
open_ capLength i t = error ("open_ pattern match failure: " <> show (capLength, i, t))


open :: Serialise a => CapLength -> Index -> BinaryTree a -> AuthPath
open capLength i = open_ capLength i . fmap hashData


verify_ :: CapLength -> CapCommitment -> Index -> AuthPath -> Commitment -> Bool
verify_ capLength c@(CapCommitment capLeaves) i p y =
  case p of
    AuthPath [] -> error "tried to verify empty path"
    AuthPath [x] ->
      let z = fromMaybe (error "capLeaves index out of range")
            $ capLeaves Tree.!! (i `quot` 2)
      in (unIndex i < 2 * unCapLength capLength)
        && z == (if i == 0 then mergeCommitments (y, x)
                 else if i == 1 then mergeCommitments (x, y)
                 else error "impossible case in MerkleTree.verify")
    AuthPath (x:xs) ->
      if i `mod` 2 == 0
      then verify_ capLength c (i `quot` 2) (AuthPath xs) (mergeCommitments (y, x))
      else verify_ capLength c (i `quot` 2) (AuthPath xs) (mergeCommitments (x, y))


verify :: Serialise a
       => CapLength
       -> CapCommitment
       -> Index
       -> AuthPath
       -> a
       -> Bool
verify capLength c i p = verify_ capLength c i p . Commitment . hashData
