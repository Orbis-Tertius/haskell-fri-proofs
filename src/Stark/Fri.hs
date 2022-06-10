module Stark.Fri
  ( numRounds
  , evalDomain
  , sampleIndex
  , sampleIndices
  , fiatShamirSeed
  , fiatShamirChallenge
  , splitAndFold
  , commitPhase
  , commitRound
  , commitCodeword
  , addCodeword
  , addCommitment
  , openCodeword
  , queryRound
  ) where


import Codec.Serialise (serialise)
import Data.Bits (shift, xor)
import Data.ByteString (ByteString, unpack)
import Data.ByteString.Lazy (toStrict)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Set (Set, size, member, insert)
import Data.Tuple.Extra (fst3)

import Stark.BinaryTree (fromList)
import Stark.FiniteField (sample)
import Stark.Fri.Types (DomainLength (..), ExpansionFactor (..), NumColinearityTests (..), Offset (..), Omega (..), RandomSeed (..), ListSize (..), ReducedListSize (..), Index (..), SampleSize (..), ReducedIndex (..), Codeword (..), ProofStream (..), Challenge (..), ProofElement (..))
import Stark.Hash (hash)
import Stark.MerkleTree (commit, open)
import Stark.Types.AuthPath (AuthPath)
import Stark.Types.Commitment (Commitment)
import Stark.Types.Scalar (Scalar)


numRounds :: DomainLength -> ExpansionFactor -> NumColinearityTests -> Int
numRounds (DomainLength d) (ExpansionFactor e) (NumColinearityTests n) =
  if fromIntegral d > e && 4 * n < d
  then 1 + numRounds
           (DomainLength (d `div` 2))
           (ExpansionFactor e)
           (NumColinearityTests n)
  else 0


evalDomain :: Offset -> Omega -> DomainLength -> [Scalar]
evalDomain (Offset o) (Omega m) (DomainLength d)
  = [o * (m ^ i) | i <- [0..d-1]]


sampleIndex :: ByteString -> ListSize -> Index
sampleIndex bs (ListSize len) =
  foldl (\acc b -> (acc `shift` 8) `xor` fromIntegral b) 0 (unpack bs)
  `mod` Index len


sampleIndices :: RandomSeed -> ListSize -> ReducedListSize -> SampleSize -> Set Index
sampleIndices seed ls rls@(ReducedListSize rls') (SampleSize n)
  | n > rls' = error "cannot sample more indices than available in last codeword"
  | n >  2 * rls' = error "not enough entropy in indices wrt last codeword"
  | otherwise =
    fromMaybe (error "the impossible has happened: sampleIndices reached the end of the list")
  . find ((>= n) . size)
  $ fst3 <$> iterate (sampleIndicesStep seed ls rls) (mempty, mempty, 0)


sampleIndicesStep :: RandomSeed
                  -> ListSize
                  -> ReducedListSize
                  -> (Set Index, Set ReducedIndex, Int)
                  -> (Set Index, Set ReducedIndex, Int)
sampleIndicesStep (RandomSeed seed) ls (ReducedListSize rls)
                  (indices, reducedIndices, counter)
  = let index = sampleIndex (hash (seed <> toStrict (serialise counter))) ls
        reducedIndex = ReducedIndex $ unIndex index `mod` rls
    in if reducedIndex `member` reducedIndices
       then (indices, reducedIndices, counter+1)
       else (insert index indices, insert reducedIndex reducedIndices, counter+1)


fiatShamirSeed :: ProofStream -> RandomSeed
fiatShamirSeed = RandomSeed . hash . toStrict . serialise


fiatShamirChallenge :: ProofStream -> Challenge
fiatShamirChallenge = Challenge . sample . unRandomSeed . fiatShamirSeed


splitAndFold :: Omega -> Offset -> Codeword -> Challenge -> Codeword
splitAndFold (Omega omega) (Offset offset) (Codeword codeword) (Challenge alpha) =
  let n = length codeword
      (l, r) = splitAt (n `quot` 2) codeword
  in Codeword $
  [ recip 2 * ( ( 1 + alpha / (offset * (omega^i)) ) * xi
              + ( 1 - alpha / (offset * (omega^i)) ) * xj )
  | (i, xi, xj) <- zip3 [(0 :: Integer)..] l r ]


addCommitment :: Commitment -> ProofStream -> ProofStream
addCommitment c (ProofStream p) = ProofStream (IsCommitment c : p)


addCodeword :: Codeword -> ProofStream -> ProofStream
addCodeword c (ProofStream p) = ProofStream (IsCodeword c : p)


commitCodeword :: Codeword -> Commitment
commitCodeword = commit . fromMaybe (error "codeword is not a binary tree") . fromList . unCodeword


openCodeword :: Codeword -> Index -> AuthPath
openCodeword (Codeword xs) (Index i) =
  open (fromIntegral i) (fromMaybe (error "codeword is not a binary tree") (fromList xs))


commitPhase :: DomainLength
            -> ExpansionFactor
            -> NumColinearityTests
            -> Omega
            -> Offset
            -> Codeword
            -> ProofStream
            -> (ProofStream, [Codeword])
commitPhase domainLength expansionFactor numColinearityTests omega offset codeword proofStream
  = let n = numRounds domainLength expansionFactor numColinearityTests
        (proofStream', codewords', codeword') =
          (iterate (commitRound omega offset) (proofStream, [], codeword))
          !! (n-1)
    in (addCodeword codeword' proofStream', codeword' : codewords')


commitRound :: Omega
            -> Offset
            -> (ProofStream, [Codeword], Codeword)
            -> (ProofStream, [Codeword], Codeword)
commitRound omega offset (proofStream, codewords, codeword) =
  let root = commitCodeword codeword
      alpha = fiatShamirChallenge proofStream
      codeword' = splitAndFold omega offset codeword alpha
  in (addCommitment root proofStream, codeword : codewords, codeword')


queryRound :: NumColinearityTests
           -> (Codeword, Codeword)
           -> [Index]
           -> ProofStream
           -> (ProofStream, [Index])
queryRound (NumColinearityTests n) (Codeword currentCodeword, Codeword nextCodeword)
           cIndices proofStream =
  let aIndices = take n cIndices
      bIndices = take n $ (+ (Index (length currentCodeword `quot` 2))) <$> cIndices
      leafProofElems = zipWith3 (\a b c -> IsCodeword (Codeword [a,b,c]))
                       ((currentCodeword !!) . fromIntegral <$> aIndices)
                       ((currentCodeword !!) . fromIntegral <$> bIndices)
                       ((nextCodeword !!) . fromIntegral <$> cIndices)
      authPathProofElems =
        IsAuthPath <$>
        ((openCodeword (Codeword currentCodeword) <$> aIndices)
         <> (openCodeword (Codeword currentCodeword) <$> bIndices)
         <> (openCodeword (Codeword nextCodeword) <$> cIndices))
  in ( ProofStream $ reverse authPathProofElems
                  <> reverse leafProofElems
                  <> unProofStream proofStream
     , aIndices <> bIndices
     )
