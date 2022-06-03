module Stark.Fri.Types
  ( Offset (Offset, unOffset)
  , Omega (Omega, unOmega)
  , DomainLength (DomainLength, unDomainLength)
  , ExpansionFactor (ExpansionFactor, unExpansionFactor)
  , NumColinearityTests (NumColinearityTests, unNumColinearityTests)
  ) where


import Stark.Types.Scalar (Scalar)


newtype Offset = Offset { unOffset :: Scalar }


newtype Omega = Omega { unOmega :: Scalar }


newtype DomainLength = DomainLength { unDomainLength :: Int }


newtype ExpansionFactor = ExpansionFactor { unExpansionFactor :: Rational }


newtype NumColinearityTests = NumColinearityTests { unNumColinearityTests :: Int }
