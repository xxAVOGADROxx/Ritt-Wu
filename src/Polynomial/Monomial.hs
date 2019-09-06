{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Polynomial.Monomial
  -- * Types
  ( Mon(..)
  ) where
import Prelude as P
import Polynomial.Exponent
import Numeric.Algebra as N
import Data.Foldable
import Data.Function
import Control.DeepSeq

-- | Constraint synonym for rings that can be used as polynomial coefficient.
class (DecidableZero r, Ring r, Commutative r, Eq r) =>   CoeffRing r
instance (DecidableZero r, Ring r, Commutative r, Eq r) => CoeffRing r

newtype Mon k ord =
  Mon
    { getMon :: (k , Exp ord)
    } deriving (Eq)

-- ----------------------<< FUNCTIONS >>--------------------
-- ----------------------<< INSTANCES >>--------------------
instance (Show k, Eq k) => Show (Mon k ord) where
  show xs = showMon (getMon xs)

showMon :: (Show k, Eq k) => (k, Exp ord) -> String
showMon m
  | list == zero = show coeff
  | otherwise = show coeff ++ show list
  where
    coeff = fst m
    list = snd m
-----------------------------------------------------------------------------------------
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
-----------------------------------------------------------------------------------------
instance (Num k) => Additive (Mon k ord) where
  (+) xs xp =  Mon $ on addPol' getMon xs xp

addPol' :: (Num k) => (k, Exp ord) -> (k, Exp ord) -> (k, Exp ord)
addPol' (a, b) (c, d) = (a P.+ c, b N.+ d) --numeric ours newtypes and prelude defined types
-----------------------------------------------------------------------------------------
instance (Num k) => Multiplicative (Mon k ord) where
  (*) xs xz = Mon $ on multPol' getMon xs xz

multPol' :: (Num k) => (k, Exp ord) -> (k, Exp ord) -> (k, Exp ord)
multPol' (a, b) (c, d) = (a P.* c, b N.* d)
-----------------------------------------------------------------------------------------
instance (Num k) => Division (Mon k ord) where
  (/) = undefined

instance (Num k) => Unital (Mon k ord) where
  one = undefined -- Mon (0, one)

instance (Num k) => Semiring (Mon k ord)
instance (Num k) => Abelian (Mon k ord)
instance (Num k) => Monoidal (Mon k ord) where
  zero = Mon (0, zero ) -- Mon (0, zero)
instance (Num k) => LeftModule Integer (Mon k ord) where
  (.*) = undefined
instance (Num k) => RightModule Integer (Mon k ord) where
  (*.) = undefined
instance (Num k) => LeftModule Natural (Mon k ord) where
  (.*) = undefined
instance (Num k) => RightModule Natural (Mon k ord) where
  (*.) = undefined
instance (Num k) => Group (Mon k ord) where
  (-)  xs xz = Mon $ on subPol' getMon xs xz

subPol' ::(Num k) => (k, Exp ord) -> (k, Exp ord) -> (k, Exp ord)
subPol' (a, b) (c, d) = (a P.- c, b N.- d)
----------------------------------------------------------------------------------
instance NFData (Mon k ord) where
  rnf x = seq x ()
-------------------------------------------------------------------------------
instance (Eq k)=> Ord (Mon k Lex) where
  compare = on compare (snd . getMon)
  (<) = on (P.<) (snd . getMon)
  (>) = on (P.>) (snd . getMon)

-- λ> a = Mon (2,m[5,2,3,2]) :: Mon Int Lex
-- λ> b = Mon (8,m[4,2,3,2]) :: Mon Int Lex
-- λ> a P.> b
-- True
-- λ>

instance (Eq k)=> Ord (Mon k Revlex) where
  compare = on compare (snd . getMon)
  (<) = on (P.<) (snd . getMon)
  (>) = on (P.>) (snd . getMon)

-- λ> a = Mon (2,m[5,2,3,2]) :: Mon Int Revlex
-- λ> b = Mon (2,m[4,2,3,6]) :: Mon Int Revlex
-- λ> b P.> a
-- True
-- λ>
