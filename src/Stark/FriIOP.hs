{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}


module Stark.FriIOP
  ( ProverState (ProverState)
  , fri
  , FriDSL (GetCommitment, GetLastCodeword, GetQueries)
  , commitPhase
  ) where


import Control.Lens ((^.), _1)
import Control.Monad (void)
import "monad-extras" Control.Monad.Extra (iterateM)
import Data.Functor ((<&>))
import Data.Kind (Type)
import Polysemy (Sem, Member, makeSem)
import Stark.Fri (emptyProofStream, addCodeword)
import Stark.Fri.Types (Challenge, ProofStream, FriConfiguration, Codeword, DomainLength (..), ExpansionFactor (..), NumColinearityTests (..), Omega (..), Offset (..), Query, AuthPaths, LastCodeword, ABC, A (..), B (..), C (..))
import Stark.Types.CapCommitment (CapCommitment)
import Stark.Types.CapLength (CapLength (..))
import Stark.Types.FiatShamir (IOP, sampleChallenge, reject)
import Stark.Types.Index (Index (..))


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


fri :: FriConfiguration
    -> Sem (FriIOP ': r) [Index]
fri = todo


todo :: a
todo = todo


newtype RoundIndex = RoundIndex { unRoundIndex :: Int }
  deriving (Eq, Ord, Num, Enum, Real, Integral)


type FriDSL :: (Type -> Type) -> Type -> Type
data FriDSL m a where
  GetConfig :: FriDSL m FriConfiguration
  GetCommitment :: RoundIndex -> FriDSL m CapCommitment
  GetLastCodeword :: FriDSL m (Maybe LastCodeword)
  GetQueries :: RoundIndex -> [ABC Index] -> FriDSL m ([Query], [AuthPaths])

makeSem ''FriDSL


commitPhase
  :: Member FriIOP r
  => Member FriDSL r
  => Sem r LastCodeword
commitPhase = do
  config <- getConfig
  let n = numRounds config
  commitments <- sequence $ commitRound <$> [0 .. RoundIndex (n-1)]
  lastCodewordM <- getLastCodeword
  -- TODO: verify commitment to last codeword
  case lastCodewordM of
    Just lastCodeword -> pure lastCodeword
    Nothing -> reject "commitPhase: no last codeword found"


commitRound
  :: Member FriIOP r
  => Member FriDSL r
  => RoundIndex
  -> Sem r CapCommitment
commitRound round = do
  alpha <- sampleChallenge
  getCommitment round


queryPhase
  :: Member FriIOP r
  => Member FriDSL r
  => [Index]
  -> Sem r ()
queryPhase indices = do
  config <- getConfig
  void (take (numRounds config) <$> iterateM queryRound (indices, 0))


queryRound
  :: Member FriIOP r
  => Member FriDSL r
  => ([Index], RoundIndex)
  -> Sem r ([Index], RoundIndex)
queryRound (indices, round) = do
  config <- getConfig
  let nextIndices = indices <&> (`mod` Index (unDomainLength
                      (roundDomainLength config (round+1))))
      aIndices = A <$> indices
      bIndices = B <$> (+ Index (unDomainLength (roundDomainLength config round) `quot` 2))
             <$> indices
      cIndices = C <$> indices
  void $ getQueries round ((,,) <$> aIndices <*> bIndices <*> cIndices)
  -- TODO: verify opening proofs & do colinearity checks
  pure (nextIndices, round+1)


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
