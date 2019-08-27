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
      getM :: (t ,Array D Ix1 t) -- a tupla that contains the coefficient k and the array of exponents
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
  show m = show (fst $ getM m) ++ (formatSS $ showMon (toList (snd $ getM m)) 0)


instance (Num t, Multiplicative t) => Multiplicative (Monomial ord t ) where
  (*) = multMon

instance (Num t) => Additive (Monomial ord t) where
  (+) = undefined
-- |
-- = Functions
--
multMon :: (Num t, Multiplicative t) => Monomial ord t -> Monomial ord t -> Monomial ord t
multMon xs xz = Monomial $ ( on (N.*) (fst . getM) xs xz,  (on (A.liftArray2 (P.+)) (snd . getM) xs xz))


showMon :: (Eq k, Show k, Num k) =>[k] -> Int -> String
showMon  (x:xs) s
  | null xs = format
  | x == 0 = printMon
  | otherwise = format  ++ printMon
  where
    next = succ
    format = "x_{" ++ show  s ++ "}^{" ++ show x ++ "}"
    printMon = showMon xs (next s)


makeMon :: (Num t ) => t -> [t] -> Monomial ord t
makeMon k xs = Monomial $ (k , makeVectorR D Par (Sz $ length xs) (xs !!))


-- :set -XDataKinds
--getComp
--setComp
-- >>> size
-- convertAs

--  evaluateM -> allows to get values from delayed arrays
-- evaluate' -> Similar to evaluateM, but will throw an exception in pure code.
-- liftArray2 -> Similar to zipWith, except dimensions of both arrays either have to be the same, or at least one of the two array must be a singleton array, in which case it will behave as a map.
-- minimuM
--maximumM
-- all -> Determines whether all element of the array satisfy the predicate.
-- any ->  Determines whether any element of the array satisfies the predicate.
-- foldlS -> (sequential ?)
--  extractFromToM  #########################
-- unconsM -> Take one element off the vector from the left side. 
newtype Exp = Exp {getf :: (Int, Array D Ix1 Ix1)}
-- la solucion es poder representarlo como tupla, la funcion unconsM no puede ser aplicado las funciones de tupla
