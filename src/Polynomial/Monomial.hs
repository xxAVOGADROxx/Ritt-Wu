{-# LANGUAGE FlexibleInstances #-}
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
   NFData(..),
   m,
   mp,
   m',
   mp'
 )
where
import Data.Massiv.Array as A
import Prelude as P
import Data.Char.SScript
import Numeric.Algebra as N
import Data.Function
import Control.DeepSeq
--import GHC.Generics (Generic, Generic1)


-- | Array monomial with representation r
type DelayArray   = Array D Ix1 Int
-- The above representation need to know the representation (r) of array.

-- | A wrapper for monomials with a certain (monomial) order
newtype Mon ord =
  Mon
  { getMon :: DelayArray
    }
  deriving (Eq)

-- * Names for orderings.
--   We didn't choose to define one single type for ordering names for the extensibility.

-- | Lexicographical order
data Lex = Lex
-- | Reverse lexicographical order
data Revlex = Revlex
-- ----------------------<< FUNCTIONS >>--------------------
-- | Monomial with terms
m :: [Int] -> Mon ord
m [0] = Mon $ makeVectorR D Par (Sz 0) id --error "A empty term should be represented as m[]"
m xs
  | xs == [] = Mon $ makeVectorR D Par (Sz 0) id
  | otherwise = Mon $ makeVectorR D Par (Sz $ length xs) (xs !!)
-----------------------------------------------------------------------------------------
-- lex order
m' :: [Int] -> Mon ord
m' = m . reverse
-----------------------------------------------------------------------------------------
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
mp' :: [Int] -> [Int] -> Mon ord
mp' xs xz
  | lxs /= lxz = error "The size of the position and exponent doesn't correspond"
  | otherwise =  m $ reverse (mpRev xs xz 1)
  where
    lxs = length xs
    lxz = length xz
-----------------------------------------------------------------------------------------
------- <<INSTANCES >>--------------
instance Show (Mon ord) where
  show m = formatSS $ showMon (A.toList (getMon m)) 1

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
  (*) xs xz = m (quitZero $ on aux (toList . getMon) xs xz)

aux :: [Int] -> [Int] -> [Int]
aux [] [] = []
aux (x:xs) [] = x : aux xs []
aux [] (y:yp) = y : aux [] yp
aux (x:xs) (y:yp) = x P.+ y : aux xs yp

quitZero :: [Int] -> [Int]
quitZero [] = []
quitZero xs
  | last xs == 0 = quitZero $ init xs
  | otherwise = xs
---------------------------------------------------------------------------------------
instance Division (Mon ord) where
  (/) xs xz = m (quitZero $on aux' (toList . getMon) xs xz)

aux' :: [Int] -> [Int] -> [Int]
aux' [] [] = []
aux' (x:xs) [] = x : aux' xs []
aux' [] (y:yp) = y : aux' [] yp
aux' (x:xs) (y:yp) = x P.- y : aux' xs yp

---------------------------------------------------------------------------------------
instance Additive (Mon ord) where
  (+) xs xz = Mon $ on verificationMl getMon xs xz
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
  (-) xs xz = Mon $ on verificationMl getMon xs xz

verificationMl :: DelayArray -> DelayArray -> DelayArray
verificationMl xs xz
  | xs == xz = xs
  | otherwise = error "The monomial doesn't match " 
---------------------------------------------------------------------------------------
instance NFData (Mon ord) where
  rnf x = seq x ()
  -- sujeto a revision cunado se realize la diviision
  -- polynomial, delay es el mas optimo porque no se realizan
  -- operaciones "estrictas sobre los monomios"
  -- si seq evalua sequencial es correcto (verificar)
   -- deberia usar force?

-- λ> let xd = m[1,2] N.* m[1,2]
-- λ> force xd
-- x₀²x₁⁴
-- λ> :sprint xd
-- xd = Exp(massiv-0.4.0.0:Data.Massiv.Array.Delayed.Pull.DArray
--                  (ParOn [])
--                  (massiv-0.4.0.0:Data.Massiv.Core.Index.Internal.SafeSz 2) _)
-- λ> 
---------------------------------------------------------------------------------------
instance Ord (Mon Lex) where
  compare = on lex' (toList . getMon)
  (>) = on (P.>) (toList . getMon)
  (<) = on (P.<) (toList . getMon)

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
  compare = on revlex' (toList . getMon)

revlex' :: (Num a, Eq a, Ord a) => [a] -> [a] -> Ordering
revlex' = on lex' reverse
---------------------------------------------------------------------------------------
instance Unital (Mon ord ) where
  one = undefined --Mon empty
-- λ> a = Mon empty
-- λ> getT a
-- Array D Seq (Sz1 0)
--   [  ]

-- λ>
