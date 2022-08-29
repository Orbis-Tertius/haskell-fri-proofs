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
import Debug.Trace (trace)

import qualified Stark.BinaryTree as Tree
import Stark.Hash (hash)
import Stark.Types.AuthPath (AuthPath (AuthPath, unAuthPath))
import Stark.Types.BinaryTree (BinaryTree (IsLeaf, IsNode))
import Stark.Types.CapCommitment (CapCommitment (CapCommitment, unCapCommitment))
import Stark.Types.CapLength (CapLength (CapLength, unCapLength))
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
  if capLength > 1
  then let f = unCapCommitment . commit_ (capLength `quot` 2)
    in CapCommitment $ IsNode (f x) (f y)
  else CapCommitment . IsLeaf $ commitCapLeaf t


commitCapLeaf :: BinaryTree MerkleHash -> Commitment
commitCapLeaf (IsLeaf x) = Commitment x
commitCapLeaf (IsNode x y) = mergeCommitments (commitCapLeaf x, commitCapLeaf y)


open__ :: Index -> BinaryTree MerkleHash -> AuthPath
open__ 0 (IsLeaf _) = AuthPath []
open__ i t@(IsNode x y) =
  let n = Index $ Tree.size t
      m = n `quot` 2
  in if i < m
     then (open__ i x) <> AuthPath [commitCapLeaf y]
     else (open__ (i-m) y) <> AuthPath [commitCapLeaf x]
open__ i t = error ("open_ pattern match failure: " <> show (i, t))


open_ :: CapLength -> Index -> BinaryTree MerkleHash -> AuthPath
open_ (CapLength capLength) i t =
    AuthPath . take ( Tree.depth t
                    - round (logBase (2 :: Double) (fromIntegral capLength)) )
  . unAuthPath $ open__ i t


open :: Serialise a => CapLength -> Index -> BinaryTree a -> AuthPath
open capLength i = open_ capLength i . fmap hashData


verify_ :: CapLength -> CapCommitment -> Index -> AuthPath -> Commitment -> Bool
verify_ capLength c@(CapCommitment capLeaves) i p y =
  case p of
    AuthPath [] ->
      let z = fromMaybe (error "capLeaves index out of range 0")
            $ capLeaves Tree.!! i
      in (unIndex i < unCapLength capLength
            || trace "index out of range" False)
         && (capLength == CapLength (Tree.size capLeaves)
             || trace "wrong CapLength" False)
         && (z == y || trace "commitment check failed" False)
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
