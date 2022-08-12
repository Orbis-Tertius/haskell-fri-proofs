{-# LANGUAGE RankNTypes #-}


module Plonk.Arithmetization
  ( columnVectorToPoly
  , circuitWithDataToPolys
  , circTraverse
  , combineCircuitPolys
  , getZerofier
  ) where


import Plonk.Types.Circuit


columnVectorToPoly
  :: Domain n a
  -> Vect n a
  -> Maybe (UnivariatePolynomial a)
columnVectorToPoly = todo


circuitWithDataToPolys
  :: Domain m a
  -> CircuitWithData ps m d a
  -> Maybe (CircuitWithData' UnivariatePolynomial ps d a)
circuitWithDataToPolys dom = circTraverse (columnVectorToPoly dom)


circTraverse :: (h a -> g (f a))
             -> CircuitWithData' h ps d a
             -> g (CircuitWithData' f ps d a)
circTraverse = todo


combineCircuitPolys
  :: CircuitWithData' UnivariatePolynomial ps d a
  -> Challenge a
  -> Maybe (UnivariatePolynomial a)
combineCircuitPolys = todo


getZerofier :: Domain n a -> UnivariatePolynomial a
getZerofier = todo


todo :: a
todo = todo
