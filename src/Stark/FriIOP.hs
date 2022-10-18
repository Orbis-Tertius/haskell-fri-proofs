{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}


module Stark.FriIOP
  (
  ) where


import Control.Lens ((^.), _1, _3)
import "monad-extras" Control.Monad.Extra (iterateM)
import Control.Monad.State (State, get, put)
import Data.Kind (Type)
import Polysemy (Sem, Member, makeSem)
import Stark.Fri (numRounds, addCommitment, commitCodeword, emptyProofStream, addCodeword, splitAndFold)
import Stark.Fri.Types (Challenge, ProofStream (..), FriConfiguration, Codeword, DomainLength, ExpansionFactor, NumColinearityTests, Omega, Offset)
import Stark.Types.CapLength (CapLength)
import Stark.Types.FiatShamir (IOP, appendToTranscript, sampleChallenge, getTranscript)
import Stark.Types.Index (Index)


type FriIOP = IOP Challenge ProofStream () ()


data ProverState =
  ProverState
  { codewords :: [Codeword] }


fri :: FriConfiguration
    -> Sem (FriIOP ': r) [Index]
fri = todo


todo :: a
todo = todo


type FriDSL :: (Type -> Type) -> Type -> Type
data FriDSL m a where
  GenerateCommitment :: ProofStream -> Challenge -> Omega -> Offset -> FriDSL m ProofStream
  GetLastCodeword :: FriDSL m (Maybe Codeword)

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
  -> Sem r ()
commitPhase domainLength expansionFactor numColinearityTests capLength omega offset = do
  let n = numRounds domainLength expansionFactor numColinearityTests capLength
  result <- iterateM (commitRound capLength)
            (emptyProofStream, omega, offset)
  lastCodewordM <- getLastCodeword
  case lastCodewordM of
    Just lastCodeword -> do
      let lastResult = result !! (length result - 1)
      appendToTranscript (addCodeword lastCodeword (lastResult ^. _1))
    Nothing -> pure ()


commitRound
  :: Member FriIOP r
  => Member FriDSL r
  => CapLength
  -> (ProofStream, Omega, Offset)
  -> Sem r (ProofStream, Omega, Offset)
commitRound capLength (proofStream, omega, offset) = do
  alpha <- sampleChallenge
  t <- generateCommitment proofStream alpha omega offset
  appendToTranscript t
  proofStream' <- getTranscript
  pure (proofStream', omega ^ two, offset ^ two)
  where two :: Integer
        two = 2

