{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Polynomial.Terms
  ( Term(..)
  ) where
import Prelude as P
import Polynomial.Monomial
import Numeric.Algebra as N
import Data.Foldable
import Data.Function
import Control.DeepSeq
import Data.Massiv.Array as A
import Data.Char.SScript
import GHC.Generics (Generic)
-- | Constraint synonym for rings that can be used as polynomial coefficient.

-- data Term k ord =
--   Term
--     { getT :: (k , Mon ord)
--     } deriving (Eq)

data  Term k ord = Term !k !(Mon ord) deriving (Eq, Generic) -- Term 4 $ m[1,2,3,4] :: Term Rational Revlex
instance (NFData k) => NFData (Term k ord) where
  rnf (Term k mon) = rnf k `deepseq` rnf mon `deepseq` ()
-- ----------------------<< FUNCTIONS >>--------------------
-- ----------------------<< INSTANCES >>--------------------
instance (Num k, Show k, Eq k) => Show (Term k ord) where
  show xs = showMon xs

showMon :: (Num k, Show k, Eq k) => Term k ord -> String
showMon (Term k mon)
  | mon == zero = show k
  | otherwise = (formatSS . show) k ++ show mon

-----------------------------------------------------------------------------------------
instance (Num k) => Additive (Term k ord) where
  (+) xs xp = addPol' xs xp

addPol' :: (Num k) => Term k mon -> Term k mon -> Term k mon
addPol' (Term k mon) (Term k' mon') = Term (k P.+ k')( mon N.+ mon')
-----------------------------------------------------------------------------------------
instance (Num k) => Multiplicative (Term k ord) where
  (*) xs xz = multPol' xs xz

multPol' :: (Num k) => Term k mon -> Term k mon -> Term k mon
multPol' (Term k mon) (Term k' mon') = Term (k P.* k')( mon N.* mon')

-----------------------------------------------------------------------------------------
instance (Fractional k, Num k) => Division (Term k ord) where
  (/) xs xp = divPol' xs xp

divPol'  :: (Fractional k, Num k) => Term k mon -> Term k mon -> Term k mon
divPol' (Term k mon) (Term k' mon') = Term (k P./ k')( mon N./ mon')

instance (Num k) => Unital (Term k ord) where
  one = undefined -- Term (0, one)

instance (Num k) => Semiring (Term k ord)
instance (Num k) => Abelian (Term k ord)
instance (Num k) => Monoidal (Term k ord) where
  zero =  Term 0 zero -- Term (0, zero)
instance (Num k) => LeftModule Integer (Term k ord) where
  (.*) = undefined
instance (Num k) => RightModule Integer (Term k ord) where
  (*.) = undefined
instance (Num k) => LeftModule Natural (Term k ord) where
  (.*) = undefined
instance (Num k) => RightModule Natural (Term k ord) where
  (*.) = undefined
instance (Num k) => Group (Term k ord) where
  (-)  xs xz = subPol' xs xz

subPol' ::(Num k) => Term k mon -> Term k mon -> Term k mon
subPol' (Term k mon) (Term k' mon') = Term (k P.- k')( mon N.- mon')

-------------------------------------------------------------------------------
--instance Eq (Term k ord) where
--  (==) =
-- instance (Eq k)=> Ord (Term k Lex) where
--   compare = on compare (snd . getT)
--   (<) = on (P.<) (snd . getT)
--   (>) = on (P.>) (snd . getT)

-- λ> a = Term (2,m[5,2,3,2]) :: Term Int Lex
-- λ> b = Term (8,m[4,2,3,2]) :: Term Int Lex
-- λ> a P.> b
-- True
-- λ>

instance (Eq k)=> Ord (Term k Revlex) where
  compare (Term k mon)(Term k' mon') = compare mon mon'
  (<) (Term k mon)(Term k' mon') = (P.<) mon mon'
  (>) (Term k mon)(Term k' mon') = (P.>) mon mon'
