{-# LANGUAGE OverloadedLabels #-}


module Spec.Gen
  ( genFriConfiguration
  , genProofStream
  , genCodeword
  , genScalar
  ) where


import Control.Lens ((^.))
import Data.Generics.Labels ()

import Spec.Prelude
import Stark.FiniteField (cardinality)
import Stark.Fri.Types (FriConfiguration (..), Codeword (..), ProofStream (..))
import Stark.Types.Scalar (Scalar (..))


genFriConfiguration :: Gen FriConfiguration
genFriConfiguration = todo


genProofStream :: FriConfiguration -> Gen ProofStream
genProofStream = todo


genCodeword :: FriConfiguration -> Gen Codeword
genCodeword config =
  Codeword <$> (sequence $ replicate
  (config ^. #domainLength . #unDomainLength)
  genScalar)


genScalar :: Gen Scalar
genScalar = Scalar . fromIntegral <$> choose (0, cardinality - 1)


todo :: a
todo = todo
