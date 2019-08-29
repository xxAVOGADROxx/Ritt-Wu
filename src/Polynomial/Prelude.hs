{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Polynomial.Prelude where
import Prelude as P
import Polynomial.Monomial
import Numeric.Algebra as N
import Data.Foldable
import Data.Function

-- | Constraint synonym for rings that can be used as polynomial coefficient.
class (DecidableZero r, Ring r, Commutative r, Eq r) =>
      CoeffRing r
instance (DecidableZero r, Ring r, Commutative r, Eq r) => CoeffRing r


newtype Polynomial k ord =
  Polynomial
    { getPol :: (k , Monomial ord)
    } --deriving (Eq)

-- class (Unital k, Show k, Eq k) => PolyContext k

instance ( Show k, Eq k) => Show (Polynomial k Lex) where
 show xs =  showPol $  (getPol xs) : []

showPol :: (Show k, Eq k) => [(k, Monomial ord)] -> String
showPol [] = " " --este caso nunca se dara en la instancia show
showPol (x:xs)
  | list == one = show coeff -- ++ show xs uncomment to eaval a list of polynomials
  | otherwise = show coeff ++ show list --  ++ show xs
  where
    coeff = fst x
    list = snd x

class (CoeffRing (Coefficient poly)) =>
      IsPolynomial poly
  where
  type Coefficient poly :: *
  --toPolynomial :: (Coefficient poly) -> poly
  --toPolynomial = undefined



class ( IsPolynomial poly, IsOrderMon (MonOrder poly)) =>
      IsOrderedPolynomial poly
  where
  type MonOrder poly :: *

-------------------------------------------------------------------
instance (IsOrderMon ord, CoeffRing k) => IsPolynomial (Polynomial k ord) where
  type Coefficient (Polynomial k ord) = k

instance (CoeffRing k, IsOrderMon ord) =>
         IsOrderedPolynomial (Polynomial k ord) where
  type MonOrder (Polynomial k ord) = ord

instance (Num k) => Additive (Polynomial k ord) where
  (+) = addPol

instance (Num k) => Multiplicative (Polynomial k ord) where
  (*) = multPol

instance (Num k) => Division (Polynomial k ord) where
  (/) = undefined

instance (Num k) => Unital (Polynomial k ord) where
  one = Polynomial (0, one) 

instance (Num k) => Semiring (Polynomial k ord)
instance (Num k) => Abelian (Polynomial k ord)
instance (Num k) => Monoidal (Polynomial k ord) where
  zero = undefined
instance (Num k) => LeftModule Integer (Polynomial k ord) where
  (.*) = undefined
instance (Num k) => RightModule Integer (Polynomial k ord) where
  (*.) = undefined
instance (Num k) => LeftModule Natural (Polynomial k ord) where
  (.*) = undefined
instance (Num k) => RightModule Natural (Polynomial k ord) where
  (*.) = undefined
instance (Num k) => Group (Polynomial k ord) where
  (-) = subPol
subPol :: (Num k) => Polynomial k ord -> Polynomial k ord -> Polynomial k ord
subPol xs xz = Polynomial $ on subPol' getPol xs xz

subPol' ::
     (Num k) => (k, Monomial ord) -> (k, Monomial ord) -> (k, Monomial ord)
subPol' (a, b) (c, d) = (a P.- c, b N.- d)

multPol :: (Num k) => Polynomial k ord -> Polynomial k ord -> Polynomial k ord
multPol xs xz = Polynomial $ on multPol' getPol xs xz

multPol' ::
     (Num k) => (k, Monomial ord) -> (k, Monomial ord) -> (k, Monomial ord)
multPol' (a, b) (c, d) = (a P.* c, b N.* d)

addPol :: (Num k) => Polynomial k ord -> Polynomial k ord -> Polynomial k ord
addPol xs xz = Polynomial $ on addPol' getPol xs xz

addPol' ::
     (Num k) => (k, Monomial ord) -> (k, Monomial ord) -> (k, Monomial ord)
addPol' (a, b) (c, d) = (a P.+ c, b N.+ d)
