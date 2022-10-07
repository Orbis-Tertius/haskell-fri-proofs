{-# LANGUAGE UndecidableInstances #-}
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
import           Data.Group                         (Group)
import           Data.Kind                          (Constraint)
import qualified Data.Map                           as Map
import           Data.Vinyl.TypeLevel               (Nat (S, Z),
                                                     NatToInt (natToInt))
import           Math.Algebra.Polynomial.Class      (AlmostPolynomial (scalarP, scaleP, sumP),
                                                     CoeffP, Polynomial (evalP),
                                                     Ring, divM, isZeroP,
                                                     monomP, monomP',
                                                     mulByMonomP, subsP, zeroP)
import           Math.Algebra.Polynomial.FreeModule (BaseF, CoeffF,
                                                     FreeMod (FreeMod),
                                                     FreeModule, findMaxTerm,
                                                     toFreeModule)
import           Math.Algebra.Polynomial.Univariate (U (U), Univariate (Uni))
import           Plonk.FFT                          (fft)
import           Plonk.Types.Circuit                (Challenge (Challenge),
                                                     Circuit,
                                                     CircuitM (CircuitM),
                                                     CircuitShape (CNil, (:&)),
                                                     ColIndex (ColIndex),
                                                     ColType (MkCol),
                                                     Domain (Domain),
                                                     GateConstraint (MkGateConstraint),
                                                     HasData (WithData), Length,
                                                     RelativeCellRef (MkRelativeCellRef),
                                                     RelativeRowIndex (RelativeRowIndex))
import           Plonk.Types.Fin                    (Fin (FS, FZ))
import           Plonk.Types.Vect                   (Vect, toList)
import           Stark.Types.UnivariatePolynomial   (UnivariatePolynomial (UnivariatePolynomial, unUnivariatePolynomial))
import Die (die)


columnVectorToPoly
  :: Group a
  => Num a
  => Domain n a
  -> Vect n a
  -> UnivariatePolynomial a
columnVectorToPoly d xs = UnivariatePolynomial . Uni . FreeMod $
  Map.fromList (zip (U <$> [0..]) (fft d (toList xs)))


circuitWithDataToPolys
  :: Group a
  => Num a
  => Domain m a
  -> Circuit ps 'WithData m d a
  -> CircuitM UnivariatePolynomial ps 'WithData d a
circuitWithDataToPolys dom = circMap (columnVectorToPoly dom)


circTraverse :: Applicative g
             => Functor f
             => Functor h
             => (h a -> g (f a))
             -> CircuitM h ps 'WithData d a
             -> g (CircuitM f ps 'WithData d a)
circTraverse k (CircuitM shape constraints) =
   flip CircuitM constraints <$> circShapeTraverse k shape

circMap
  :: Functor f
  => Functor g
  => (f a -> g a)
  -> CircuitM f ps 'WithData d a
  -> CircuitM g ps 'WithData d a
circMap k (CircuitM shape constraints) = CircuitM (circShapeMap k shape) constraints

circShapeMap
  :: Functor f
  => Functor g
  => (f a -> g a)
  -> CircuitShape f ps 'WithData d a
  -> CircuitShape g ps 'WithData d a
circShapeMap _ CNil      = CNil
circShapeMap q (x :& xs) = mapI q x :& circShapeMap q xs

mapI
 :: Functor f
 => Functor g
 => (f a -> g a)
 -> Compose f Identity a
 -> Compose g Identity a
mapI f (Compose q) = Compose $ fmap Identity . f . fmap runIdentity $ q


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
  :: Ring a
  => RelativeCellRefToPoly ps n
  => Domain d a
  -> CircuitShape UnivariatePolynomial ps 'WithData d a
  -> GateConstraint n d a
  -> UnivariatePolynomial a
plugInDataToGateConstraint omega shape (MkGateConstraint poly) =
  evalP scalarP (relativeCellRefToPoly omega shape) poly


type RelativeCellRefToPoly :: [ColType] -> Nat -> Constraint
class RelativeCellRefToPoly ps n where
  relativeCellRefToPoly
    :: Ring a
    => Domain d a
    -> CircuitShape UnivariatePolynomial ps 'WithData d a
    -> RelativeCellRef n
    -> UnivariatePolynomial a

instance RelativeCellRefToPoly '[] 'Z where
  relativeCellRefToPoly _ CNil _ = die "impossible: relativeCellRefToPoly _ CNil _"

instance RelativeCellRefToPoly xs z => RelativeCellRefToPoly ('MkCol j k ': xs) ('S z) where
  relativeCellRefToPoly
    omega
    (col0 :& _)
    (MkRelativeCellRef i (ColIndex FZ)) =
    rotateColPoly omega i (runIdentity <$> getCompose col0)
  relativeCellRefToPoly
    omega
    (_ :& cols)
    (MkRelativeCellRef i (ColIndex (FS n))) =
      relativeCellRefToPoly omega cols
        (MkRelativeCellRef i (ColIndex n))


rotateColPoly
  :: Ring a
  => Domain d a
  -> RelativeRowIndex
  -> UnivariatePolynomial a
  -> UnivariatePolynomial a
rotateColPoly (Domain omega) (RelativeRowIndex i) =
  subsP (const (scaleP (omega i) (monomP (U 1))))


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
  => RelativeCellRefToPoly ps n
  => Ring a
  => Domain d a
  -> CircuitM UnivariatePolynomial ps 'WithData d a
  -> Challenge a
  -> UnivariatePolynomial a
combineCircuitPolys omega (CircuitM shape gates) challenge =
  linearlyCombineGatePolys challenge $
    plugInDataToGateConstraint omega shape <$> gates


getZerofier
  :: forall n a.
     NatToInt n
  => Num a
  => Group a
  => Domain n a
  -> UnivariatePolynomial a
getZerofier d@(Domain q) = UnivariatePolynomial . Uni . FreeMod $
  Map.fromList (zip (U <$> [0..]) (fft d (q <$> [0 .. natToInt @n - 1])))


-- returns the quotient if the denominator
-- perfectly divides the numerator.
divUniPoly :: Ring a
           => Fractional a
           => UnivariatePolynomial a
           -> UnivariatePolynomial a
           -> Maybe (UnivariatePolynomial a)
divUniPoly = divideMaybe


polynomialLongDivision :: forall p. (Polynomial p, Fractional (CoeffP p)) => p -> p -> (p,p)
polynomialLongDivision p0 q = go zeroP p0 where

  (bq,cq) = case findMaxTerm' q of
    Just bc -> bc
    Nothing -> die "polynomialLongDivision: division by zero"

  go !acc !p = case findMaxTerm' p of
    Nothing      -> (acc,zeroP)
    Just (bp,cp) -> case divM bp bq of
      Nothing      -> (acc,p)
      Just br      -> let cr = (cp / cq)
                          u  = scaleP cr (mulByMonomP br q)
                          p' = p - u
                          acc' = (acc + monomP' br cr)
                      in  go acc' p'

divideMaybe :: (Polynomial p, Fractional (CoeffP p)) => p -> p -> Maybe p
divideMaybe p q = case polynomialLongDivision p q of
  (s,r) -> if isZeroP r then Just s else Nothing


findMaxTerm' :: FreeModule f => f -> Maybe (BaseF f, CoeffF f)
findMaxTerm' = findMaxTerm . toFreeModule

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
