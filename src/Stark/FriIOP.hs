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
import "monad-extras" Control.Monad.Extra (iterateM)
import Data.Kind (Type)
import Polysemy (Sem, Member, makeSem)
import Stark.Fri (numRounds, emptyProofStream, addCodeword)
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


type FriDSL :: (Type -> Type) -> Type -> Type
data FriDSL m a where
  GetCommitment :: Challenge -> Omega -> Offset -> FriDSL m CapCommitment
  GetLastCodeword :: FriDSL m (Maybe LastCodeword)
  GetQueries :: RoundIndex -> Index -> FriDSL m ([Query], [AuthPaths])

makeSem ''FriDSL


commitPhase
  :: Member FriIOP r
  => Member FriDSL r
  => DomainLength
  -> ExpansionFactor
  -> NumColinearityTests
  -> CapLength
  -> Omega
  -> Offset
  -> Sem r LastCodeword
commitPhase domainLength expansionFactor numColinearityTests capLength omega offset = do
  let n = numRounds domainLength expansionFactor numColinearityTests capLength
  result <- take n <$> iterateM commitRound
            (omega, offset)
  lastCodewordM <- getLastCodeword
  case lastCodewordM of
    Just lastCodeword -> pure lastCodeword
    Nothing -> reject


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
  => [Index] -> Sem r ()
queryPhase = todo


queryRound
  :: Member FriIOP r
  => Member FriDSL r
  => [Index]
  -> Sem r ()
queryRound = todo
