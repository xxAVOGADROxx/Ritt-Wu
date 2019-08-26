{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
-- Module      : Data.Massiv.Array
-- Copyright   :
-- License     : BSD3
-- Maintainer  : Jose Seraquive <jose.seraquive@gmail.com>
-- Stability   : experimental
-- Portability : non-portable

-}


module Polynomial.Monomial
   -- * Types
  ( Monomial(..),
   OrderMon(..),
    -- * Classes
    MonomialOrder,
    -- *Functions
    makeMon,
    multMon
 )
where
import Data.Massiv.Array as A
import Prelude as P hiding ((*))
import Data.Char.SScript
import Numeric.Algebra as N
import Data.Function
import Criterion.Main


newtype Monomial (ord :: OrderMon) t =
  Monomial
    { -- | mon is and vector of coefficient and exponent of a given monomial
      mon :: Array D Ix1 t
    }

data OrderMon =
  -- | Lexicographic order
   Lex
  -- | Reverse lexicographic order
  | Revlex


-- |
-- = Class
--
class MonomialOrder ord t where
  compareM :: Monomial ord t -> Monomial ord t -> Ordering


instance (Eq t, Num t, Show t) => Show (Monomial ord t ) where
  show m = formatSS $ showMon (toList (mon m)) 0


instance (Num t) => Multiplicative (Monomial ord t ) where
  (*) = multMon

instance (Num t) => Additive (Monomial ord t) where
  (+) = undefined
-- |
-- = Functions
--
multMon :: (Num t) => Monomial ord t -> Monomial ord t -> Monomial ord t
multMon xs xz = Monomial $ on (A.zipWith (P.+)) ( aux . mon)  xs xz

aux :: (Num t) => Array D Ix1 t -> Array D Ix1 t
aux xs = extract' 1 (Sz $ elemsCount xs P.- 1) xs 


showMon :: (Eq k, Show k, Num k) =>[k] -> Int -> String
showMon  (x:xs) s
  | null xs = format
  | x == 0 = printMon
  | s == 0 = show x ++ printMon
  | otherwise = format  ++ printMon
  where
    next = succ
    format = "x_{" ++ show  s ++ "}^{" ++ show x ++ "}"
    printMon = showMon xs (next s)


makeMon :: (Num t ) => [t] -> Monomial a t
makeMon xs = Monomial $ makeVectorR D Par (Sz $ length xs) (xs !!)


-- :set -XDataKinds
--getComp
--setComp
-- >>> size
-- convert




