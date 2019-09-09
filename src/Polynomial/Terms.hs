{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Polynomial.Terms
  -- * Types
  ( Term(..)
  ) where
import Prelude as P
import Polynomial.Monomial
import Numeric.Algebra as N
import Data.Foldable
import Data.Function
import Control.DeepSeq

-- | Constraint synonym for rings that can be used as polynomial coefficient.

class (DecidableZero r, Ring r, Commutative r, Eq r) =>   CoeffRing r
instance (DecidableZero r, Ring r, Commutative r, Eq r) => CoeffRing r

newtype Term k ord =
  Term
    { getT :: (k , Mon ord)
    } deriving (Eq)

-- ----------------------<< FUNCTIONS >>--------------------
-- ----------------------<< INSTANCES >>--------------------
instance (Num k, Show k, Eq k) => Show (Term k ord) where
  show xs = showMon (getT xs)

showMon :: (Num k, Show k, Eq k) => (k, Mon ord) -> String
showMon m
  | list == zero = show coeff
  | otherwise = show coeff ++ show list
  where
    coeff = abs $fst m
    list = snd m
-----------------------------------------------------------------------------------------
-- class (CR (Coefficient poly)) =>
--       IsMonomial poly
--   where
--   type Coefficient poly :: *
--   --toMonomial :: (Coefficient poly) -> poly
  --toMonomial = undefined



-- class ( IsMonomial poly, IsOrderMon (MonOrder poly)) =>
--       IsOrderedMonomial poly
--   where
--   type MonOrder poly :: *

-- -------------------------------------------------------------------
-- instance (IsOrderMon ord, CR k) => IsMonomial (Mon k ord) where
--   type Coefficient (Mon k ord) = k

-- instance (CR k, IsOrderMon ord) =>
--          IsOrderedMonomial (Mon k ord) where
--   type MonOrder (Mon k ord) = ord
-----------------------------------------------------------------------------------------
instance (Num k) => Additive (Term k ord) where
  (+) xs xp =  Term $ on addPol' getT xs xp

addPol' :: (Num k) => (k, Mon ord) -> (k, Mon ord) -> (k, Mon ord)
addPol' (a, b) (c, d) =(a P.+ c, b N.+ d) --numeric ours newtypes and prelude defined types
-----------------------------------------------------------------------------------------
instance (Num k) => Multiplicative (Term k ord) where
  (*) xs xz = Term $ on multPol' getT xs xz

multPol' :: (Num k) => (k, Mon ord) -> (k, Mon ord) -> (k, Mon ord)
multPol' (a, b) (c, d) = (a P.* c, b N.* d)
-----------------------------------------------------------------------------------------
instance (Fractional k, Num k) => Division (Term k ord) where
  (/) xs xp = Term $ on divPol' getT xs xp

divPol' :: (Fractional k, Num k )=>(k, Mon ord) -> (k, Mon ord) -> (k, Mon ord)
divPol' (a,b) (c,d)= (a P./ c, b N./ d )

instance (Num k) => Unital (Term k ord) where
  one = undefined -- Term (0, one)

instance (Num k) => Semiring (Term k ord)
instance (Num k) => Abelian (Term k ord)
instance (Num k) => Monoidal (Term k ord) where
  zero = Term (0, zero ) -- Term (0, zero)
instance (Num k) => LeftModule Integer (Term k ord) where
  (.*) = undefined
instance (Num k) => RightModule Integer (Term k ord) where
  (*.) = undefined
instance (Num k) => LeftModule Natural (Term k ord) where
  (.*) = undefined
instance (Num k) => RightModule Natural (Term k ord) where
  (*.) = undefined
instance (Num k) => Group (Term k ord) where
  (-)  xs xz = Term $ on subPol' getT xs xz

subPol' ::(Num k) => (k, Mon ord) -> (k, Mon ord) -> (k, Mon ord)
subPol' (a, b) (c, d) = (a P.- c, b N.- d)
----------------------------------------------------------------------------------
instance NFData (Term k ord) where
  rnf x = seq x ()
-------------------------------------------------------------------------------
--instance Eq (Term k ord) where
--  (==) = 
instance (Eq k)=> Ord (Term k Lex) where
  compare = on compare (snd . getT)
  (<) = on (P.<) (snd . getT)
  (>) = on (P.>) (snd . getT)

-- λ> a = Term (2,m[5,2,3,2]) :: Term Int Lex
-- λ> b = Term (8,m[4,2,3,2]) :: Term Int Lex
-- λ> a P.> b
-- True
-- λ>

instance (Eq k)=> Ord (Term k Revlex) where
  compare = on compare (snd . getT)
  (<) = on (P.<) (snd . getT)
  (>) = on (P.>) (snd . getT)

-- λ> a = Term (2,m[5,2,3,2]) :: Term Int Revlex
-- λ> b = Term (2,m[4,2,3,6]) :: Term Int Revlex
-- λ> b P.> a
-- True
-- λ>
---instance Num (Term k ord) 
