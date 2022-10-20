{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}


module Stark.FriIOP
  ( FriResponse (Commit, LastCodeword', QueryRound)
  , FriIOP
  , ProverState (ProverState)
  , FriDSL (GetCommitment, GetLastCodeword, GetQueries)
  , getCommitment
  , getQueries
  , getLastCodeword
  , RoundIndex (RoundIndex, unRoundIndex)
  , fri
  , commitPhase
  ) where


import Codec.Serialise (Serialise, serialise)
import Control.Lens ((^.))
import Data.ByteString.Lazy (toStrict)
import Data.List.Extra ((!?))
import Control.Monad (void, when)
import "monad-extras" Control.Monad.Extra (iterateM)
import Data.Functor ((<&>))
import Data.Kind (Type)
import Polysemy (Sem, Member, makeSem)
import Stark.Fri (sampleIndex, commitCodeword)
import Stark.Fri.Types (Challenge, FriConfiguration, Codeword, DomainLength (DomainLength, unDomainLength), ExpansionFactor (ExpansionFactor), NumColinearityTests (NumColinearityTests, unNumColinearityTests), Query, AuthPaths, LastCodeword (unLastCodeword), ABC, A (A), B (B), C (C), RandomSeed (RandomSeed), ListSize (ListSize), ReducedListSize (ReducedListSize, unReducedListSize), SampleSize (SampleSize), ReducedIndex (ReducedIndex))
import Stark.Hash (hash)
import Stark.Types.CapCommitment (CapCommitment)
import Stark.Types.CapLength (CapLength (CapLength))
import Stark.Types.FiatShamir (IOP, sampleChallenge, reject)
import Stark.Types.Index (Index (Index, unIndex))


type FriResponse :: Type
data FriResponse =
    Commit CapCommitment
  | LastCodeword' LastCodeword
  | QueryRound ([Query], [AuthPaths])


type FriIOP :: (Type -> Type) -> Type -> Type
type FriIOP = IOP Challenge FriResponse


type ProverState :: Type
data ProverState =
  ProverState [Codeword]


type RoundIndex :: Type
newtype RoundIndex = RoundIndex { unRoundIndex :: Int }
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral)


type FriDSL :: (Type -> Type) -> Type -> Type
data FriDSL m a where
  GetConfig :: FriDSL m FriConfiguration
  GetCommitment :: RoundIndex -> Challenge -> FriDSL m CapCommitment
  GetLastCodeword :: FriDSL m (Maybe LastCodeword)
  GetQueries :: RoundIndex -> [ABC Index] -> FriDSL m ([Query], [AuthPaths])

makeSem ''FriDSL


fri :: Member FriIOP r
    => Member FriDSL r
    => Sem r ()
fri = do
  config <- getConfig
  (_lastCodeword, commitments, alphas) <- commitPhase
  lastCommitPhaseAlpha <- maybe (reject "no commitPhase alphas") pure
    $ alphas !? (length alphas - 1)
  let n = numRounds config
      indices = sampleIndices
        (randomSeed lastCommitPhaseAlpha)
        (ListSize (unDomainLength (roundDomainLength config 1)))
        (ReducedListSize (unDomainLength
          (roundDomainLength config (RoundIndex n))))
        (SampleSize (unNumColinearityTests (config ^. #numColinearityTests)))
  queryPhase commitments alphas indices


sampleIndices
  :: RandomSeed
  -> ListSize
  -> ReducedListSize
  -> SampleSize
  -> [(Index, ReducedIndex)]
sampleIndices (RandomSeed seed) ls rls (SampleSize sampleSize) =
  [ (index, reducedIndex)
  | counter <- [1 .. sampleSize]
  , let index = sampleIndex (hash (seed <> toStrict (serialise counter))) ls
  , let reducedIndex = ReducedIndex $ unIndex index `mod` unReducedListSize rls
  ]


randomSeed :: Serialise a => a -> RandomSeed
randomSeed = RandomSeed . hash . toStrict . serialise


commitPhase
  :: Member FriIOP r
  => Member FriDSL r
  => Sem r (LastCodeword, [CapCommitment], [Challenge])
commitPhase = do
  config <- getConfig
  let n = numRounds config
  (commitments, alphas) <- unzip
    <$> sequence (commitRound <$> [0 .. RoundIndex (n-1)])
  lastRoot <- maybe (reject "commitPhase: no last root") pure
    $ commitments !? (length commitments - 1)
  lastCodewordM <- getLastCodeword
  case lastCodewordM of
    Just lastCodeword -> do
      when (lastRoot /= commitCodeword (config ^. #capLength)
                        (unLastCodeword lastCodeword))
        $ reject "commitPhase: last codeword commitment check failed"
      pure (lastCodeword, commitments, alphas)
    Nothing -> reject "commitPhase: no last codeword"


commitRound
  :: Member FriIOP r
  => Member FriDSL r
  => RoundIndex
  -> Sem r (CapCommitment, Challenge)
commitRound i = do
  alpha <- sampleChallenge
  (, alpha) <$> getCommitment i alpha


queryPhase
  :: Member FriDSL r
  => Member FriIOP r
  => [CapCommitment]
  -> [Challenge]
  -> [(Index, ReducedIndex)]
  -> Sem r ()
queryPhase commitments challenges indices = do
  config <- getConfig
  void (take (numRounds config) <$>
    iterateM queryRound (indices, 0, commitments, challenges))


queryRound
  :: Member FriIOP r
  => Member FriDSL r
  => ([(Index, ReducedIndex)], RoundIndex, [CapCommitment], [Challenge])
  -> Sem r ([(Index, ReducedIndex)], RoundIndex, [CapCommitment], [Challenge])
queryRound (indices, i, (_root:nextRoot:remainingRoots), (_alpha:alphas)) = do
  config <- getConfig
  let nextIndices = (fst <$> indices) <&> (`mod` Index (unDomainLength
                      (roundDomainLength config (i + 1))))
      aIndices = A . fst <$> indices
      bIndices = B . (+ Index (unDomainLength (roundDomainLength config i)
                                                `quot` 2))
             . fst <$> indices
      cIndices = C . fst <$> indices
  void $ getQueries i ((,,) <$> aIndices <*> bIndices <*> cIndices)
  -- TODO: verify opening proofs & do colinearity checks
  pure ( zip nextIndices (snd <$> indices)
       , i + 1
       , nextRoot:remainingRoots
       , alphas)
queryRound (_, _, _:[], _) = reject "queryRound: not enough roots (1)"
queryRound (_, _, [], _) = reject "queryRound: not enough roots (0)"
queryRound (_, _, _, []) = reject "queryRound: not enough alphas"


roundDomainLength
  :: FriConfiguration
  -> RoundIndex
  -> DomainLength
roundDomainLength config i =
  (config ^. #domainLength) `quot` (2 ^ i)


numRounds :: FriConfiguration -> Int
numRounds config =
  numRounds'
  (config ^. #domainLength)
  (config ^. #expansionFactor)
  (config ^. #numColinearityTests)
  (config ^. #capLength)


numRounds' :: DomainLength -> ExpansionFactor -> NumColinearityTests -> CapLength -> Int
numRounds' (DomainLength d) (ExpansionFactor e) (NumColinearityTests n) (CapLength n') =
  if fromIntegral d > e && d > n' && 4 * n < d
  then 1 + numRounds'
           (DomainLength (d `div` 2))
           (ExpansionFactor e)
           (NumColinearityTests n)
           (CapLength n')
  else 0
