{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Polynomial.Prelude where 
import Polynomial.Monomial
import Numeric.Algebra

-- | Constrain synonym for rings that can be used as polynomial coefficient
class (DecidableZero r, Ring r, Commutative r, Eq r) => CoeffRing r
instance (DecidableZero r, Ring r, Commutative r, Eq r) => CoeffRing r

pseudoDivision :: Rational -> Rational
pseudoDivision = undefined
