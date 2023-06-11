{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-|
-- Module      : Data.Massiv.Array
-- Copyright   :
-- License     : BSD3
-- Maintainer  : Jose Seraquive <jose.seraquive@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
-}
module Polynomial.Monomial
  (
   -- * Types
   Mon(..),
   Lex(..),
   Revlex(..),
    -- * Classes

    -- * Functions
   m,
   mp,

 )
where
import Data.Massiv.Array as A
import Prelude as P
import Data.Char.SScript
import Numeric.Algebra as N
import Data.Function
import Control.DeepSeq
import GHC.Generics (Generic, Generic1)


-- | A wrapper for monomials with a certain (monomial) order
newtype Mon ord = Mon (Array P Ix1 Int) deriving (Generic, NFData, Eq)
--instance NFData (Mon ord) where
--  rnf x = seq x ()

-- * Names for orderings.
--   We didn't choose to define one single type for ordering names for the extensibility.

-- | Lexicographical order
data Lex = Lex
-- | Reverse lexicographical order
data Revlex = Revlex

---multiDeg :: Mon m -> Mon m -> [Int]
-- ----------------------<< FUNCTIONS >>--------------------
-- | Monomial with terms
m :: [Int] -> Mon ord
m [] = Mon $ A.fromList (ParN 1) []
m [0] = Mon $ A.fromList (ParN 1) [] --error "A empty term should be represented as m[]"
m xs = Mon $ A.fromList (ParN 1) xs

-- Function that recive the x_i position with the corresponding exp
-- | Monomial with the term position
mp :: [Int] -> [Int] -> Mon ord
mp xs xz
  | lxs /= lxz = error "The size of the position and exponent doesn't correspond"
  | otherwise =  m $ mpRev xs xz 1
  where
    lxs = length xs
    lxz = length xz

mpRev :: [Int] -> [Int] -> Int -> [Int]
mpRev [] [] _ = []
mpRev (p:ps) (x:xs) n
  | p == 0 = error "The initial monomial position is 1 not 0"
  | n /= p = 0 : mpRev (p : ps) (x : xs) next
  | otherwise = x : mpRev ps xs next
  where
    next = n P.+ 1
-----------------------------------------------------------------------------------------
-- lex order
-- mp' :: [Int] -> [Int] -> Mon ord
-- mp' xs xz
--   | lxs /= lxz = error "The size of the position and exponent doesn't correspond"
--   | otherwise =  m $ reverse (mpRev xs xz 1)
--   where
--     lxs = length xs
--     lxz = length xz
-----------------------------------------------------------------------------------------
------- <<INSTANCES >>--------------
instance Show (Mon ord) where
  show (Mon m) = formatSS $ showMon (A.toList m) 1

showMon :: (Num a, Eq a, Ord a, Show a) => [a] -> Int -> String
showMon [] _ = ""
showMon (x:xs) s
  | x == 0 = printMon
  | null xs = format
  | otherwise = format ++ printMon
  where
    next = succ
    format = "x_{" ++ show s ++ "}^{" ++ show x ++ "}"
    printMon = showMon xs (next s)
---------------------------------------------------------------------------------------
instance Multiplicative (Mon ord) where
  (*) (Mon n) (Mon n') = m (quitZero $ on aux toList n n')

aux :: [Int] -> [Int] -> [Int]
aux [] [] = []
aux (x:xs) [] = x : aux xs []
aux [] (y:yp) = y : aux [] yp
aux (x:xs) (y:yp) = x P.+ y : aux xs yp

quitZero :: [Int] -> [Int]
quitZero [] = []
quitZero xs
  | last xs == 0 = quitZero $ P.init xs
  | otherwise = xs
---------------------------------------------------------------------------------------
instance Division (Mon ord) where
  (/) (Mon n) (Mon n') = m (quitZero $ on aux' (A.toList) n n')

aux' :: [Int] -> [Int] -> [Int]
aux' [] [] = []
aux' (x:xs) [] = x : aux' xs []
aux' [] (y:yp) = y : aux' [] yp
aux' (x:xs) (y:yp) = x P.- y : aux' xs yp

---------------------------------------pal mar------------------------------------------------
instance Additive (Mon ord) where
  (+) (Mon m)(Mon m') = Mon $ verificationMl m m'
---------------------------------------------------------------------------------------
instance Semiring (Mon ord)
instance Abelian (Mon ord)
instance Monoidal (Mon ord) where
  zero = Mon empty
instance LeftModule Integer (Mon ord) where
  (.*) = undefined
instance RightModule Integer (Mon ord) where
  (*.) = undefined
instance LeftModule Natural (Mon ord) where
  (.*) = undefined
instance RightModule Natural (Mon ord) where
  (*.) = undefined
---------------------------------------------------------------------------------------
instance Group (Mon ord) where
  (-) (Mon m)(Mon m') = Mon $ verificationMl m m'

verificationMl :: Array P Ix1 Int -> Array P Ix1 Int -> Array P Ix1 Int
verificationMl xs xz
  | xs == xz = xs
  | otherwise = error "The monomial doesn't match "
---------------------------------------------------------------------------------------
  -- sujeto a revision cunado se realize la diviision
  -- polynomial, delay es el mas optimo porque no se realizan
  -- operaciones "estrictas sobre los monomios"
  -- si seq evalua sequencial es correcto (verificar)
   -- deberia usar force?


---------------------------------------------------------------------------------------
instance Ord (Mon Lex) where
  compare (Mon m)(Mon m') = on lex' A.toList m m'
  (>) (Mon m)(Mon m')= on (P.>) A.toList m m'
  (<) (Mon m)(Mon m')= on (P.<) A.toList m m'

lex' :: (Num a, Eq a, Ord a) => [a] -> [a] -> Ordering
lex' [] [] = EQ
lex' [] _ = LT
lex' _ [] = GT
lex' (x:xs) (y:ys)
  | x == y = lex' xs ys
  | x P.> y = GT
  | otherwise = LT
---------------------------------------------------------------------------------------
instance Ord (Mon Revlex) where
  compare (Mon m)(Mon m')= on revlex' A.toList m m'

revlex' :: (Num a, Eq a, Ord a) => [a] -> [a] -> Ordering
revlex' = on lex' P.reverse
---------------------------------------------------------------------------------------
instance Unital (Mon ord ) where
  one = undefined --Mon empty
