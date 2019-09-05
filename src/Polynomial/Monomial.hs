{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Polynomial.Monomial(
  -- * Types
  Mon(..),

                         ) where
import Prelude as P
import Polynomial.Exponent
import Numeric.Algebra as N
import Data.Foldable
import Data.Function
import Control.DeepSeq

-- | Constraint synonym for rings that can be used as polynomial coefficient.
class (DecidableZero r, Ring r, Commutative r, Eq r) =>
      CoeffRing r
instance (DecidableZero r, Ring r, Commutative r, Eq r) => CoeffRing r


newtype Mon k ord =
  Mon
    { getMon :: (k , Exp ord)
    } --deriving (Eq)

-- class (Unital k, Show k, Eq k) => PolyContext k

instance ( Show k, Eq k) => Show (Mon k Lex) where
 show xs =  showPol  [getMon xs]

showPol :: (Show k, Eq k) => [(k, Exp ord)] -> String
showPol [] = " " --este caso nunca se dara en la instancia show
showPol (x:xs)
  | list == one = show coeff -- ++ show xs uncomment to eaval a list of polynomials
  | otherwise = show coeff ++ show list --  ++ show xs
  where
    coeff = fst x
    list = snd x

class (CoeffRing (Coefficient poly)) =>
      IsMonomial poly
  where
  type Coefficient poly :: *
  --toMonomial :: (Coefficient poly) -> poly
  --toMonomial = undefined



-- class ( IsMonomial poly, IsOrderMon (MonOrder poly)) =>
--       IsOrderedMonomial poly
--   where
--   type MonOrder poly :: *

-- -------------------------------------------------------------------
-- instance (IsOrderMon ord, CoeffRing k) => IsMonomial (Mon k ord) where
--   type Coefficient (Mon k ord) = k

-- instance (CoeffRing k, IsOrderMon ord) =>
--          IsOrderedMonomial (Mon k ord) where
--   type MonOrder (Mon k ord) = ord

instance (Num k) => Additive (Mon k ord) where
  (+) = addPol

instance (Num k) => Multiplicative (Mon k ord) where
  (*) = multPol

instance (Num k) => Division (Mon k ord) where
  (/) = undefined

instance (Num k) => Unital (Mon k ord) where
  one = Mon (0, one) 

instance (Num k) => Semiring (Mon k ord)
instance (Num k) => Abelian (Mon k ord)
instance (Num k) => Monoidal (Mon k ord) where
  zero = undefined
instance (Num k) => LeftModule Integer (Mon k ord) where
  (.*) = undefined
instance (Num k) => RightModule Integer (Mon k ord) where
  (*.) = undefined
instance (Num k) => LeftModule Natural (Mon k ord) where
  (.*) = undefined
instance (Num k) => RightModule Natural (Mon k ord) where
  (*.) = undefined
instance (Num k) => Group (Mon k ord) where
  (-) = subPol


instance NFData (Mon k ord) where
  rnf x = seq x ()



subPol :: (Num k) => Mon k ord -> Mon k ord -> Mon k ord
subPol xs xz = Mon $ on subPol' getMon xs xz

subPol' ::
     (Num k) => (k, Exp ord) -> (k, Exp ord) -> (k, Exp ord)
subPol' (a, b) (c, d) = (a P.- c, b N.- d)

multPol :: (Num k) => Mon k ord -> Mon k ord -> Mon k ord
multPol xs xz = Mon $ on multPol' getMon xs xz

multPol' ::
     (Num k) => (k, Exp ord) -> (k, Exp ord) -> (k, Exp ord)
multPol' (a, b) (c, d) = (a P.* c, b N.* d)

addPol :: (Num k) => Mon k ord -> Mon k ord -> Mon k ord
addPol xs xz = Mon $ on addPol' getMon xs xz

addPol' ::
     (Num k) => (k, Exp ord) -> (k, Exp ord) -> (k, Exp ord)
addPol' (a, b) (c, d) = (a P.+ c, b N.+ d)




