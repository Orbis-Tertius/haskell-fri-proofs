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
import Data.Kind (Type)
import Polysemy (Sem, Member, makeSem)
import Stark.Fri (emptyProofStream, addCodeword)
import Stark.Fri.Types (Challenge, ProofStream, FriConfiguration, Codeword, DomainLength, ExpansionFactor, NumColinearityTests, Omega, Offset, Query, AuthPaths, LastCodeword)
import Stark.Types.CapCommitment (CapCommitment)
import Stark.Types.CapLength (CapLength)
import Stark.Types.FiatShamir (IOP, sampleChallenge, reject)
import Stark.Types.Index (Index)


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


newtype RoundIndex = RoundIndex Int
  deriving (Eq, Ord, Num)


type FriDSL :: (Type -> Type) -> Type -> Type
data FriDSL m a where
  GetCommitment :: Challenge -> Omega -> Offset -> FriDSL m CapCommitment
  GetLastCodeword :: FriDSL m (Maybe LastCodeword)
  GetQueries :: RoundIndex -> Index -> FriDSL m ([Query], [AuthPaths])

makeSem ''FriDSL


commitPhase
  :: Member FriIOP r
  => Member FriDSL r
  => FriConfiguration
  -> Sem r LastCodeword
commitPhase config = do
  let n = numRounds config
  result <- take n <$> iterateM commitRound
            (config ^. #omega, config ^. #offset)
  lastCodewordM <- getLastCodeword
  case lastCodewordM of
    Just lastCodeword -> pure lastCodeword
    Nothing -> reject "commitPhase: no last codeword found"


commitRound
  :: Member FriIOP r
  => Member FriDSL r
  => (Omega, Offset)
  -> Sem r (Omega, Offset)
commitRound (omega, offset) = do
  alpha <- sampleChallenge
  _ <- getCommitment alpha omega offset
  pure (omega ^ two, offset ^ two)
  where two :: Integer
        two = 2


queryPhase
  :: Member FriIOP r
  => Member FriDSL r
  => FriConfiguration
  -> [Index]
  -> Sem r ()
queryPhase config indices =
  void (take (numRounds config) <$> iterateM queryRound (indices, 0))


queryRound
  :: Member FriIOP r
  => Member FriDSL r
  => ([Index], RoundIndex)
  -> Sem r ([Index], RoundIndex)
queryRound = todo


numRounds :: FriConfiguration -> Int
numRounds = todo
