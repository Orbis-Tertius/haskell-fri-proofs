{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}


module Plonk.Arithmetization
  ( columnVectorToPoly
  , circuitWithDataToPolys
  , circTraverse
  , circShapeTraverse
  , combineCircuitPolys
  , getZerofier
  , divUniPoly
  ) where


import           Data.Functor.Compose               (Compose (Compose),
                                                     getCompose)
import           Data.Functor.Identity              (Identity (Identity),
                                                     runIdentity)
import           Data.Kind                          (Constraint)
import           Data.Vinyl.TypeLevel               (Nat (S, Z))
import           Math.Algebra.Polynomial.Class      (AlmostPolynomial (scalarP, scaleP, sumP),
                                                     Polynomial (evalP), Ring,
                                                     monomP, subsP)
import           Math.Algebra.Polynomial.Univariate (U (U))
import           Plonk.Types.Circuit                (Challenge (Challenge),
                                                     Circuit,
                                                     Circuit' (Circuit),
                                                     CircuitShape (CNil, (:&)),
                                                     ColIndex (ColIndex),
                                                     ColType (MkCol), Domain,
                                                     DomainGenerator (DomainGenerator),
                                                     Fin (FS, FZ),
                                                     GateConstraint (GateConstraint),
                                                     HasData (WithData), Length,
                                                     RelativeCellRef (RelativeCellRef),
                                                     RelativeRowIndex (RelativeRowIndex),
                                                     Vect)
import           Stark.Types.Scalar                 (Scalar)
import           Stark.Types.UnivariatePolynomial   (UnivariatePolynomial (UnivariatePolynomial, unUnivariatePolynomial))


columnVectorToPoly
  :: Domain n a
  -> Vect n a
  -> Maybe (UnivariatePolynomial a)
columnVectorToPoly = todo -- this can be done with an FFT (Faez)


circuitWithDataToPolys
  :: Domain m a
  -> Circuit ps 'WithData m d a
  -> Maybe (Circuit' UnivariatePolynomial ps 'WithData d a)
circuitWithDataToPolys dom = circTraverse (columnVectorToPoly dom)


circTraverse :: Applicative g
             => Functor f
             => Functor h
             => (h a -> g (f a))
             -> Circuit' h ps 'WithData d a
             -> g (Circuit' f ps 'WithData d a)
circTraverse k (Circuit shape constraints) =
   flip Circuit constraints <$> circShapeTraverse k shape


circShapeTraverse :: Applicative g
                  => Functor f
                  => Functor h
                  => (h a -> g (f a))
                  -> CircuitShape h ps 'WithData d a
                  -> g (CircuitShape f ps 'WithData d a)
circShapeTraverse _ CNil = pure CNil
circShapeTraverse k (x :& xs) =
  (:&) <$> wrapInIdentity k x <*> circShapeTraverse k xs


wrapInIdentity
  :: Functor g
  => Functor f
  => Functor h
  => (h a -> g (f a))
  -> (Compose h Identity) a -> g ((Compose f Identity) a)
wrapInIdentity f (Compose xs) =
  Compose . fmap Identity
    <$> f (runIdentity <$> xs)


plugInDataToGateConstraint
  :: Ring a => Foo ps n
  => DomainGenerator a
  -> CircuitShape UnivariatePolynomial ps 'WithData d a
  -> GateConstraint n d a
  -> UnivariatePolynomial a
plugInDataToGateConstraint omega shape (GateConstraint poly) =
  evalP scalarP (relativeCellRefToPoly omega shape) poly


type Foo :: [ColType] -> Nat -> Constraint
class Foo ps n where
  relativeCellRefToPoly
    :: Ring a
    => DomainGenerator a
    -> CircuitShape UnivariatePolynomial ps 'WithData d a
    -> RelativeCellRef n -> UnivariatePolynomial a

instance Foo '[] 'Z where
  relativeCellRefToPoly _ _ _ = 1 -- impossible


instance Foo xs z => Foo ('MkCol j k ': xs) ('S z) where
  relativeCellRefToPoly
    omega
    (col0 :& _)
    (RelativeCellRef i (ColIndex FZ)) =
    rotateColPoly omega i (runIdentity <$> getCompose col0)
  relativeCellRefToPoly
    omega
    (_ :& cols)
    (RelativeCellRef i (ColIndex (FS n))) =
      relativeCellRefToPoly omega cols
        (RelativeCellRef i (ColIndex n))


rotateColPoly
  :: Ring a
  => DomainGenerator a
  -> RelativeRowIndex
  -> UnivariatePolynomial a
  -> UnivariatePolynomial a
rotateColPoly (DomainGenerator omega) (RelativeRowIndex i) =
  subsP (const (scaleP (omega ^ i) (monomP (U 1))))


linearlyCombineGatePolys
  :: Ring a
  => Challenge a
  -> [UnivariatePolynomial a]
  -> UnivariatePolynomial a
linearlyCombineGatePolys (Challenge gamma) polys =
  UnivariatePolynomial . sumP
    $ zipWith scaleP (iterate (* gamma) 1) (unUnivariatePolynomial <$> polys)


combineCircuitPolys
  :: n ~ Length ps
  => Foo ps n => Ring a
  => DomainGenerator a
  -> Circuit' UnivariatePolynomial ps 'WithData d a
  -> Challenge a
  -> UnivariatePolynomial a
combineCircuitPolys omega (Circuit shape gates) challenge =
  linearlyCombineGatePolys challenge $
    plugInDataToGateConstraint omega shape <$> gates


getZerofier :: Domain n a -> UnivariatePolynomial a
getZerofier = todo -- this can be done with an FFT (Faez)


-- returns the quotient if the denominator
-- perfectly divides the numerator.
divUniPoly :: UnivariatePolynomial Scalar
           -> UnivariatePolynomial Scalar
           -> Maybe (UnivariatePolynomial a)
divUniPoly = todo -- can be done with Euclidean algorithm


-- IOP idea:
--  * Given a root of unity which generates a domain, d
--  * Given a circuit with data, c
--     * It expresses a true statement; its constraints are satisfied
--  * Verifier provides a challenge alpha
--  * Make the polynomial p(x) = combineCircuitPolys c alpha
--  * Make the zerofier z(x) = getZerofier d
--  * Make the quotient q(x) = p(x) `divUniPoly` z(x)
--  * Commit to p(x) and q(x), send commitments to verifier
--  * Verifier provides a challenge zeta
--  * Want to show: p(zeta) = q(zeta) * z(zeta)
--  * So, prover sends openings q(zeta) and p(zeta)
--  * Verifier checks the equation
--  * Repeat until verifier is convinced
--
-- zk-SNARK protocol: apply the Fiat-Shamir transform to the IOP
--  * Faez has been working on abstraction for doing this
--    (applying Fiat-Shamir to an IOP)


todo :: a
todo = todo
