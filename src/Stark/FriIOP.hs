{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}


module Stark.FriIOP
  (
  ) where


import Control.Lens ((^.), _1, _3)
import "monad-extras" Control.Monad.Extra (iterateM)
import Control.Monad.State (State, get, put)
import Polysemy (Sem, Member)
import Stark.Fri (numRounds, addCommitment, commitCodeword, emptyProofStream, addCodeword, splitAndFold)
import Stark.Fri.Types (Challenge, ProofStream (..), FriConfiguration, Codeword, DomainLength, ExpansionFactor, NumColinearityTests, Omega, Offset)
import Stark.Types.CapLength (CapLength)
import Stark.Types.FiatShamir (IOP, appendToTranscript, sampleChallenge, getTranscript)
import Stark.Types.Index (Index)


type FriIOP = IOP Challenge ProofStream () () (State ProverState)


data ProverState =
  ProverState
  { codewords :: [Codeword] }


fri :: FriConfiguration
    -> Sem (FriIOP ': r) [Index]
fri = todo


-- commitPhase
--   :: Member FriIOP r
--   => DomainLength
--   -> ExpansionFactor
--   -> NumColinearityTests
--   -> CapLength
--   -> Omega
--   -> Offset
--   -> Sem r ()
-- commitPhase domainLength expansionFactor numColinearityTests capLength omega offset =
--   let n = numRounds domainLength expansionFactor numColinearityTests capLength
--   in appendToTranscript @Challenge @ProofStream @() @() $ do
--        ProverState codewords <- get
--        case codewords of
--          [codeword] -> do
--            result <- take n <$>
--                iterateM (commitRound capLength)
--                (emptyProofStream, [], codeword, omega, offset)
--            let lastOfResult = result !! length codewords - 1
--                proofStream = lastOfResult ^. _1
--                codewords = (^. _3) <$> result
--                finalCodeword = lastOfResult ^. _3
--                proofStream' = addCodeword finalCodeword proofStream
--            put (ProverState codewords)
--            pure $ foldl (flip addCommitment) proofStream'
--              (commitCodeword capLength <$> codewords)
--          _ -> error "commitPhase: expected exactly one codeword"


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
  -- appendToTranscript $ do
  --   ProverState codewords <- get
  --   let lastCodeword = codewords !! (length codewords - 1)
  --       newCodeword = splitAndFold omega offset lastCodeword alpha
  --   put (ProverState (codewords ++ [newCodeword]))
  --   pure (addCommitment
  --          (commitCodeword capLength newCodeword)
  --          proofStream)
  proofStream' <- getTranscript
  pure (proofStream', omega ^ two, offset ^ two)
  where two :: Integer
        two = 2



todo :: a
todo = todo
