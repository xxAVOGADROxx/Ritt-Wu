{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

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
   Monomial(..),
   --OrdMon(..),
   Lex(..),
   Revlex(..),
    -- * Classes
   IsOrderMon(..),
    -- * Functions
   NFData(..),
   m,
   multMon,
   divMon,

 )
where
import Data.Massiv.Array as A
import Prelude as P
import Data.Char.SScript
import Numeric.Algebra as N
import Data.Function
import Control.DeepSeq
--import GHC.Generics (Generic, Generic1)


-- | array monomial with representation r
type Mon   = Array D Ix1 Double
-- The above representation need to know the representation (r) of array.

-- | A wrapper for monomials with a certain (monomial ) order
newtype Monomial ord =
  Monomial
    { getMon :: Mon
    }  deriving (Eq)


m :: [Double] -> Monomial ord
m xs = Monomial $ makeVectorR D Par (Sz $ length xs) (xs !!)

totalDegree ::   Monomial ord  -> Double
totalDegree = A.sum . getMon 
{-# INLINE totalDegree #-}


instance  Show (Monomial ord) where
  show m = formatSS $ showMon (A.toList (getMon m)) 0

instance Multiplicative (Monomial ord) where
  (*) = multMon

instance Division (Monomial ord) where
  (/) = divMon

instance Additive (Monomial ord) where
  (+) = addMon

--class (LeftModule Integer r, RightModule Integer r, Monoidal r) => Group r where

instance Semiring (Monomial ord)
instance Abelian (Monomial ord)
instance Monoidal (Monomial ord) where
  zero = undefined
instance LeftModule Integer (Monomial ord)  where
  (.*) = undefined
instance RightModule Integer (Monomial ord)  where
  (*.) = undefined
instance LeftModule Natural (Monomial ord) where
  (.*) = undefined
instance RightModule Natural (Monomial ord) where
  (*.) = undefined

instance Group (Monomial ord) where
  (-) = subMon

instance NFData (Monomial ord) where
  rnf x = seq x () -- sujeto a revision cunado se realize la diviision
                  -- polynomial, delay es el mas optimo porque no se realizan
                  -- operaciones "estrictas sobre los monomios"
          -- si seq evalua sequencial es correcto (verificar)

subMon :: Monomial ord -> Monomial ord -> Monomial ord
subMon xs xz = Monomial $ on verificationMl getMon xs xz

instance Unital (Monomial ord ) where
  one = Monomial empty

addMon :: Monomial ord -> Monomial ord -> Monomial ord
addMon xs xz =  Monomial $ on verificationMl (getMon ) xs xz

verificationMl :: Array D Ix1 Double -> Array D Ix1 Double -> Array D Ix1 Double
verificationMl xs xz
  | xs == xz = xs
  | otherwise = throw $ SizeElementsMismatchException (size xs) (size xz)

showMon :: [Double] -> Int -> String
showMon [] _ = "Empty Monomial"
showMon (x:xs) s
  | null xs = format
  | otherwise = format ++ printMon
  where
    next = succ
    format = "x_{" ++ show s ++ "}^{" ++ show x ++ "}"
    printMon = showMon xs (next s)


multMon ::  Monomial ord  -> Monomial ord  -> Monomial ord
multMon xs xz = Monomial $ on (A.liftArray2 (P.+))(getMon) xs xz

divMon ::  Monomial ord  -> Monomial ord  -> Monomial ord
divMon xs xz = Monomial $ on (A.liftArray2 (P.-))(getMon) xs xz

-- * Names for orderings.
--   We didn't choose to define one single type for ordering names for the extensibility.
-- | Lexicographical order

--data OrdMon = Lex | Revlex
data Lex = Lex
data Revlex = Revlex
------------------------------------------------------------------------------------------------------------
class IsOrderMon ord  where
  compMon :: Monomial ord  -> Monomial ord -> Ordering


instance IsOrderMon Lex where
    compMon = on lex' (toList . getMon)

instance IsOrderMon Revlex where
    compMon = on revlex' (toList . getMon)

instance (IsOrderMon ord) => Ord (Monomial ord) where
    compare = undefined -- compareMonomial


lex' :: [Double] -> [Double] -> Ordering
lex' [] [] = EQ
lex' [] _ = LT
lex' _ [] = GT
lex' (x:xs) (y:ys)
  | x == y = lex' xs ys
  | x P.> y = GT
  | otherwise = LT


revlex' :: (Num a, Eq a, Ord a) => [a] -> [a] -> Ordering
revlex' [] [] = EQ
revlex' [] _ = LT
revlex' _ [] = GT
revlex' x y
  | (xr == 0 && yr == 0) || xr == yr = revlex' (reverse xrs) (reverse yrs)
  | xr P.> yr = GT
  | otherwise = LT
  where
    (xr:xrs) = reverse x
    (yr:yrs) = reverse y

-- >>> m[]
