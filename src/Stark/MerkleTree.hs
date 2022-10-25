module Stark.MerkleTree
  ( commit,
    commit_,
    open_,
    open,
    verify_,
    verify,
  )
where

import Codec.Serialise (Serialise, serialise)
import Crypto.Number.Basic (log2)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import Die (die)
import qualified Stark.BinaryTree as Tree
import Stark.Cast (intToWord64, word64ToInteger)
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
    then
      let f = unCapCommitment . commit_ (capLength `quot` 2)
       in CapCommitment $ IsNode (f x) (f y)
    else CapCommitment . IsLeaf $ commitCapLeaf t

commitCapLeaf :: BinaryTree MerkleHash -> Commitment
commitCapLeaf (IsLeaf x) = Commitment x
commitCapLeaf (IsNode x y) = mergeCommitments (commitCapLeaf x, commitCapLeaf y)

open__ :: Index -> BinaryTree MerkleHash -> AuthPath
open__ 0 (IsLeaf _) = AuthPath []
open__ i t@(IsNode x y) =
  let n = Index $ intToWord64 (Tree.size t)
      m = n `quot` 2
      yc = commitCapLeaf y
      xc = commitCapLeaf x
      h = mergeCommitments (xc, yc)
   in if m == 1
        then (if i == 0
              then AuthPath [(yc, h)]
              else AuthPath [(xc, h)])
        else if i < m
        then open__ i x <> AuthPath [(yc, h)]
        else open__ (i - m) y <> AuthPath [(xc, h)]
open__ i t = die ("open_ pattern match failure: " <> show (i, t))

open_ :: CapLength -> Index -> BinaryTree MerkleHash -> AuthPath
open_ (CapLength capLength) i t =
  AuthPath . take (Tree.depth t - log2 (word64ToInteger capLength))
    . unAuthPath
    $ open__ i t

open :: Serialise a => CapLength -> Index -> BinaryTree a -> AuthPath
open capLength i = open_ capLength i . fmap hashData

verify_ :: CapLength -> CapCommitment -> Index -> AuthPath -> Commitment -> Bool
verify_ capLength c@(CapCommitment capLeaves) i p y =
  case p of
    AuthPath [] ->
      let z =
            fromMaybe (die "capLeaves index out of range 0") $
              capLeaves `Tree.at` i
       in ( unIndex i < unCapLength capLength
              || trace ("index out of range: " <> show i <> " should be <" <> show capLength) False
          )
            && ( capLength == CapLength (intToWord64 (Tree.size capLeaves))
                   || trace
                     ( "wrong CapLength: " <> " expected " <> show capLength <> " but got "
                         <> show (Tree.size capLeaves)
                     )
                     False
               )
            && (z == y || trace "commitment check failed" False)
    AuthPath ((x,h) : xs) ->
      trace ("even? " <> show (even i))
       $ if even i
        then let h' = mergeCommitments (y,x)
          in trace (show (i, length xs, h == h'))
            $ h == h' && verify_ capLength c (i `quot` 2) (AuthPath xs) h'
        else let h' = mergeCommitments (x,y)
          in trace (show (i, length xs, h == h'))
           $ h == h' && verify_ capLength c (i `quot` 2) (AuthPath xs) h'

verify ::
  Serialise a =>
  CapLength ->
  CapCommitment ->
  Index ->
  AuthPath ->
  a ->
  Bool
verify capLength c i p = verify_ capLength c i p . Commitment . hashData
