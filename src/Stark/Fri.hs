{-# LANGUAGE OverloadedLabels #-}
module Stark.Fri
  ( getMaxDegree
  , numRounds
  , evalDomain
  , getCodeword
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
  , addQueries
  , addAuthPaths
  , openCodeword
  , queryRound
  , queryPhase
  , emptyProofStream
  , prove

  , getLastOmega
  , getLastOffset
  , getAlphas
  , verify
  ) where


import           Codec.Serialise                  (serialise)
import           Control.Lens                     ((^.))
import           Data.Bits                        (shift, xor)
import           Data.ByteString                  (ByteString, unpack)
import           Data.ByteString.Lazy             (toStrict)
import           Data.Generics.Labels             ()
import           Data.List                        (find, inits, zip4, zip5)
import qualified Data.List.Safe                   as L
import           Data.Maybe                       (fromMaybe)
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import           Data.Tuple.Extra                 (fst3, snd3, thd3)
import           Debug.Trace                      (trace)

import           Stark.BinaryTree                 (fromList)
import           Stark.FiniteField                (sample)
import           Stark.Fri.Types                  (A (A, unA),
                                                   AuthPaths (AuthPaths, unAuthPaths),
                                                   B (B, unB), C (C, unC),
                                                   Challenge (Challenge, unChallenge),
                                                   Codeword (Codeword, unCodeword),
                                                   DomainLength (DomainLength, unDomainLength),
                                                   ExpansionFactor (ExpansionFactor),
                                                   FriConfiguration (FriConfiguration),
                                                   ListSize (ListSize),
                                                   NumColinearityTests (NumColinearityTests, unNumColinearityTests),
                                                   Offset (Offset, unOffset),
                                                   Omega (Omega, unOmega),
                                                   ProofStream (ProofStream),
                                                   Query (Query, unQuery),
                                                   RandomSeed (RandomSeed, unRandomSeed),
                                                   ReducedIndex (ReducedIndex),
                                                   ReducedListSize (ReducedListSize),
                                                   SampleSize (SampleSize))
import           Stark.Hash                       (hash)
import qualified Stark.MerkleTree                 as Merkle
import           Stark.Prelude                    (uncurry4)
import           Stark.Types.AuthPath             (AuthPath)
import           Stark.Types.CapCommitment        (CapCommitment)
import           Stark.Types.CapLength            (CapLength (CapLength))
import           Stark.Types.Index                (Index (Index, unIndex))
import           Stark.Types.Scalar               (Scalar)
import           Stark.Types.UnivariatePolynomial (UnivariatePolynomial)
import           Stark.UnivariatePolynomial       (areColinear, degree,
                                                   evaluate, interpolate)


getMaxDegree :: DomainLength -> Int
getMaxDegree (DomainLength d) = floor (logBase 2 (fromIntegral d) :: Double)


numRounds :: DomainLength -> ExpansionFactor -> NumColinearityTests -> CapLength -> Int
numRounds (DomainLength d) (ExpansionFactor e) (NumColinearityTests n) (CapLength n') =
  if fromIntegral d > e && d > n' && 4 * n < d
  then 1 + numRounds
           (DomainLength (d `div` 2))
           (ExpansionFactor e)
           (NumColinearityTests n)
           (CapLength n')
  else 0


evalDomain :: Offset -> Omega -> DomainLength -> [Scalar]
evalDomain (Offset o) (Omega m) (DomainLength d)
  = [o * (m ^ i) | i <- [0..d-1]]


getCodeword :: FriConfiguration -> UnivariatePolynomial Scalar -> Codeword
getCodeword config poly =
  Codeword $ evaluate poly <$> evalDomain (config ^. #offset) (config ^. #omega) (config ^. #domainLength)


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
  . find ((>= n) . Set.size)
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
    in if reducedIndex `Set.member` reducedIndices
       then (indices, reducedIndices, counter + 1)
       else ( Set.insert index indices
            , Set.insert reducedIndex reducedIndices, counter + 1 )


fiatShamirSeed :: ProofStream -> RandomSeed
fiatShamirSeed = RandomSeed . hash . toStrict . serialise


fiatShamirChallenge :: ProofStream -> Challenge
fiatShamirChallenge = Challenge . sample . unRandomSeed . fiatShamirSeed


splitAndFold :: Omega -> Offset -> Codeword -> Challenge -> Codeword
splitAndFold (Omega omega) (Offset offset) (Codeword codeword) (Challenge alpha) =
  let (l, r) = splitAt (length codeword `quot` 2) codeword
  in Codeword $
  [ recip 2 * ( ( 1 + alpha / (offset * (omega ^ i)) ) * xi
              + ( 1 - alpha / (offset * (omega ^ i)) ) * xj )
  | (i, xi, xj) <- zip3 [(0 :: Integer)..] l r ]


emptyProofStream :: ProofStream
emptyProofStream = ProofStream [] [] Nothing []


addCommitment :: CapCommitment -> ProofStream -> ProofStream
addCommitment c (ProofStream commitments queries codewords authPaths)
  = ProofStream (commitments ++ [c]) queries codewords authPaths


addQueries :: [Query] -> ProofStream -> ProofStream
addQueries q (ProofStream commitments queries codewords authPaths)
  = ProofStream commitments (queries ++ [q]) codewords authPaths


addCodeword :: Codeword -> ProofStream -> ProofStream
addCodeword c (ProofStream commitments queries Nothing authPaths)
  = ProofStream commitments queries (Just c) authPaths
addCodeword _ _ = error "tried to add the last codeword but it is already present"


addAuthPaths :: [AuthPaths] -> ProofStream -> ProofStream
addAuthPaths ps (ProofStream commitments queries codewords authPaths)
  = ProofStream commitments queries codewords (authPaths ++ [ps])


commitCodeword :: CapLength -> Codeword -> CapCommitment
commitCodeword capLength
  = Merkle.commit capLength
  . fromMaybe (error "codeword is not a binary tree")
  . fromList
  . unCodeword


openCodeword :: CapLength -> Codeword -> Index -> AuthPath
openCodeword capLength (Codeword xs) (Index i) =
  Merkle.open capLength (fromIntegral i) (fromMaybe (error "codeword is not a binary tree") (fromList xs))


commitPhase :: DomainLength
            -> ExpansionFactor
            -> NumColinearityTests
            -> CapLength
            -> Omega
            -> Offset
            -> Codeword
            -> (ProofStream, [Codeword])
commitPhase domainLength expansionFactor numColinearityTests capLength omega offset codeword
  = let n = numRounds domainLength expansionFactor numColinearityTests capLength
        (proofStream', codewords', codeword', _, _) =
          fromMaybe (error "could not find last commit round") $
          iterate (commitRound capLength) (emptyProofStream, [], codeword, omega, offset)
          L.!! (n-1)
    in ( addCodeword codeword'
         ( addCommitment (commitCodeword capLength codeword')
           proofStream' )
       , codewords' ++ [codeword'] )


commitRound :: CapLength
            -> (ProofStream, [Codeword], Codeword, Omega, Offset)
            -> (ProofStream, [Codeword], Codeword, Omega, Offset)
commitRound capLength (proofStream, codewords, codeword, omega, offset) =
  let root = commitCodeword capLength codeword
      proofStream' = addCommitment root proofStream
      alpha = fiatShamirChallenge proofStream'
      codeword' = splitAndFold omega offset codeword alpha
  in ( proofStream'
     , codewords ++ [codeword]
     , codeword'
     , omega ^ two, offset ^ two
     )
  where two :: Integer
        two = 2


queryRound :: CapLength
           -> (Codeword, Codeword)
           -> [Index]
           -> ProofStream
           -> ProofStream
queryRound capLength (Codeword currentCodeword, Codeword nextCodeword)
           cIndices proofStream =
  let aIndices = cIndices
      bIndices = (+ Index (length currentCodeword `quot` 2)) <$> cIndices
      leafProofElems = fromMaybe (error $ "missing leaf: " <> show (length currentCodeword, length nextCodeword, cIndices, aIndices, bIndices)) <$>
         zipWith3 (\a b c -> Query <$> ((,,) <$> (A <$> a) <*> (B <$> b) <*> (C <$> c)))
         ((currentCodeword L.!!) <$> aIndices)
         ((currentCodeword L.!!) <$> bIndices)
         ((nextCodeword L.!!) <$> cIndices)
      authPathProofElems =
        ( \(a, b, c) -> AuthPaths
          ( A $ openCodeword capLength (Codeword currentCodeword) a
          , B $ openCodeword capLength (Codeword currentCodeword) b
          , C $ openCodeword capLength (Codeword nextCodeword) c
          )
        ) <$> zip3 aIndices bIndices cIndices
  in addAuthPaths authPathProofElems
     (addQueries leafProofElems proofStream)


queryPhase :: CapLength -> [Codeword] -> [Index] -> ProofStream -> ProofStream
queryPhase capLength codewords indices proofStream =
  snd3 . fromMaybe (error "could not find last query round")
    $ iterate f (indices, proofStream, 0) L.!! max 0 (length codewords - 2)
  where
    f :: ([Index], ProofStream, Int) -> ([Index], ProofStream, Int)
    f (indices', proofStream', i) =
      ( (`mod` Index (length (unCodeword (e 1 (codewords L.!! (i + 1)))) `quot` 2))
        <$> indices'
      , queryRound capLength (e 2 (codewords L.!! i), e 3 (codewords L.!! (i + 1))) indices' proofStream'
      , i + 1
      )

    e :: Int -> Maybe a -> a
    e x = fromMaybe (error ("missing codeword " <> show x))


prove :: FriConfiguration -> Codeword -> (ProofStream, [Index])
prove (FriConfiguration offset omega domainLength expansionFactor numColinearityTests capLength) codeword
  | unDomainLength domainLength == length (unCodeword codeword) =
    let (proofStream0, codewords) =
          commitPhase domainLength expansionFactor numColinearityTests capLength
                      omega offset codeword
        indices = Set.elems $ sampleIndices
          (fiatShamirSeed proofStream0)
          (ListSize (length (unCodeword (fromMaybe (error "missing second codeword") (codewords L.!! (1 :: Int))))))
          (ReducedListSize (length (unCodeword (fromMaybe (error "missing last codeword") (codewords L.!! (length codewords - 1))))))
          (SampleSize (unNumColinearityTests numColinearityTests))
        proofStream1 = queryPhase capLength codewords indices proofStream0
    in (proofStream1, indices)
  | otherwise = error "domain length does not match length of initial codeword"


getLastOmega :: FriConfiguration -> Omega
getLastOmega config =
  let nr = numRounds (config ^. #domainLength) (config ^. #expansionFactor) (config ^. #numColinearityTests) (config ^. #capLength)
  in (config ^. #omega) ^ (2 * (nr - 1))


getLastOffset :: FriConfiguration -> Offset
getLastOffset config =
  let nr = numRounds (config ^. #domainLength) (config ^. #expansionFactor) (config ^. #numColinearityTests) (config ^. #capLength)
  in (config ^. #offset) ^ (2 * (nr - 1))


-- Takes the list of commitments from the proof stream and provides
-- the list of corresponding challenges.
getAlphas :: [CapCommitment] -> [Challenge]
getAlphas roots =
  fiatShamirChallenge . (\cs -> ProofStream cs [] Nothing []) <$> tail (inits roots)


-- Returns evaluations of the polynomial at the indices if the proof is valid, or Nothing otherwise.
verify :: FriConfiguration -> ProofStream -> Bool
verify config proofStream =
  let roots = proofStream ^. #commitments
      alphas = getAlphas roots
      lastOmega = getLastOmega config
      lastOffset = getLastOffset config
      nr = numRounds (config ^. #domainLength) (config ^. #expansionFactor) (config ^. #numColinearityTests) (config ^. #capLength)
      lastRoot = fromMaybe (error "could not find last root") $ roots L.!! (length roots - 1)
  in case (proofStream ^. #lastCodeword, roots) of
    (Just lastCodeword, _:_) ->
      let lastCodewordLength = length (unCodeword lastCodeword)
          lastDomain = [ unOffset lastOffset * (unOmega lastOmega ^ i)
                       | i <- [0 .. lastCodewordLength - 1] ]
          poly = interpolate (zip lastDomain (unCodeword lastCodeword))
          maxDegree = getMaxDegree (config ^. #domainLength)
          dl = unDomainLength (config ^. #domainLength)
          nt = unNumColinearityTests (config ^. #numColinearityTests)
          topLevelIndices =
            Set.elems $ sampleIndices
              (fiatShamirSeed
                (ProofStream (proofStream ^. #commitments) [] (Just lastCodeword) []))
              (ListSize $ dl `shift` negate 1)
              (ReducedListSize $ dl `shift` negate (nr - 1))
              (SampleSize nt)
      in if lastRoot /= commitCodeword capLength lastCodeword || degree poly > maxDegree
         then trace (if lastRoot == commitCodeword capLength lastCodeword
                     then "degree poly > maxDegree"
                     else "lastRoot /= commitCodeword lastCodeword")
              False
         else and $
              [ verifyRound config topLevelIndices r alpha rootPair q p
              | (r, alpha, rootPair, q, p) <-
                  zip5 [0 .. nr - 2]
                       alphas
                       (zip roots (drop 1 roots))
                       (proofStream ^. #queries)
                       (proofStream ^. #authPaths)
              ]
    _ -> trace "missing last codeword or empty roots" False
  where
    capLength = config ^. #capLength


verifyRound :: FriConfiguration
            -> [Index]
            -> Int
            -> Challenge
            -> (CapCommitment, CapCommitment)
            -> [Query]
            -> [AuthPaths]
            -> Bool
verifyRound config topLevelIndices r alpha (root, nextRoot) qs ps =
  let omega = (config ^. #omega) ^ ((2 :: Integer) ^ r)
      offset = (config ^. #offset) ^ ((2 :: Integer) ^ r)
      dl = config ^. #domainLength . #unDomainLength
      cIndices = (`mod` fromIntegral (dl `shift` negate (r + 1))) <$> topLevelIndices
      aIndices = cIndices
      bIndices = (+ fromIntegral (dl `shift` negate (r + 1))) <$> aIndices
      ays = fst3 . unQuery <$> qs
      bys = snd3 . unQuery <$> qs
      cys = thd3 . unQuery <$> qs
      f :: Integral x => x -> Scalar
      f = (* unOffset offset) . (unOmega omega ^)
      colinearityChecks = all areColinear
        $ (\(a,b,c) -> [a,b,c])
        <$> zip3 (zip (f <$> aIndices) (unA <$> ays))
                 (zip (f <$> bIndices) (unB <$> bys))
                 (zip (repeat (unChallenge alpha)) (unC <$> cys))
      allPaths = unAuthPaths <$> ps
      aPaths = unA . fst3 <$> allPaths
      bPaths = unB . snd3 <$> allPaths
      cPaths = unC . thd3 <$> allPaths
      aAuthPathChecks = all (uncurry4 (Merkle.verify capLength))
        $ zip4 (repeat root) aIndices aPaths (unA <$> ays)
      bAuthPathChecks = all (uncurry4 (Merkle.verify capLength))
        $ zip4 (repeat root) bIndices bPaths (unB <$> bys)
      cAuthPathChecks = all (uncurry4 (Merkle.verify capLength))
        $ zip4 (repeat nextRoot) cIndices cPaths (unC <$> cys)
      authPathChecks = aAuthPathChecks && bAuthPathChecks && cAuthPathChecks
  in (colinearityChecks && authPathChecks) ||
     if colinearityChecks
          then trace ("auth path check failed: " <> show (aAuthPathChecks, bAuthPathChecks, cAuthPathChecks)) False
          else trace "colinearity check failed" False
  where
    capLength = config ^. #capLength
