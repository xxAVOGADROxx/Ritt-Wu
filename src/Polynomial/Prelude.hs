{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

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
  | otherwise =show coeff  ++ show list --  ++ show xs
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
instance (IsOrderMon ord, CoeffRing k) =>
         IsPolynomial (Polynomial k ord) where
  type Coefficient (Polynomial k ord) = k

instance (CoeffRing k, IsOrderMon ord) =>
         IsOrderedPolynomial (Polynomial k ord) where
  type MonOrder (Polynomial k ord) = ord


instance (Num k) => Additive (Polynomial k ord) where
    (+) = addPol

addPol :: (Num k)=>  Polynomial k ord -> Polynomial k ord -> Polynomial k ord
addPol xs xz = Polynomial $ on addPol' getPol xs xz

addPol' :: (Num k ) =>(k, Monomial ord) -> (k, Monomial ord) -> (k, Monomial ord)
addPol' (a,b) (c,d) = (a P.+ c, b N.+ d)
