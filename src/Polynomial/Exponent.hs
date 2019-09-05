{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

{-|
-- Module      : Data.Massiv.Array
-- Copyright   :
-- License     : BSD3
-- Maintainer  : Jose Seraquive <jose.seraquive@gmail.com>
-- Stability   : experimental
-- Portability : non-portable

-}


module Polynomial.Exponent
  (
   -- * Types
   Exp(..),
   ExpA(..),
   --OrdMon(..),
   Lex(..),
   Revlex(..),
    -- * Classes

    -- * Functions
   NFData(..),
   m,
   mp,
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
type ExpA   = Array D Ix1 Int
-- The above representation need to know the representation (r) of array.

-- | A wrapper for monomials with a certain (monomial ) order
newtype Exp ord =
  Exp
    { getExp :: ExpA
    }
  deriving (Eq)

-- ----------------------<< FUNCTIONS >>--------------------
m :: [Int] -> Exp ord
m xs = Exp $ makeVectorR D Par (Sz $ length xs) (xs !!)

-- Function that recive the x_i position with the corresponding exp

-- | Monomial with position
mp :: [Int] -> [Int] -> Exp ord
mp xs xz
  | lxs /= lxz = throw $ SizeMismatchException (Sz lxs ) (Sz lxz)
  | otherwise =  m $ mp' xs xz 1
  where
    lxs = length xs
    lxz = length xz

mp' :: [Int] -> [Int] -> Int -> [Int]
mp' [] [] _ = []
mp' (p:ps) (x:xs) n
  | p == 0 = [99999999] -- no se puede crear en la posicion 0 ya que la lista empieza en 1 
  | n /= p = 0 : mp' (p : ps) (x : xs) next
  | otherwise = x : mp' (ps) (xs) next
  where
    next = n P.+ 1


totalDegree ::   Exp ord  -> Int
totalDegree = A.sum . getExp 
{-# INLINE totalDegree #-}


instance  Show (Exp ord) where
  show m = formatSS $ showMon (A.toList (getExp m)) 1

instance Multiplicative (Exp ord) where
  (*) = multMon

instance Division (Exp ord) where
  (/) = divMon

instance Additive (Exp ord) where
  (+) = addMon

--class (LeftModule Integer r, RightModule Integer r, Monoidal r) => Group r where

instance Semiring (Exp ord)
instance Abelian (Exp ord)
instance Monoidal (Exp ord) where
  zero = undefined
instance LeftModule Integer (Exp ord)  where
  (.*) = undefined
instance RightModule Integer (Exp ord)  where
  (*.) = undefined
instance LeftModule Natural (Exp ord) where
  (.*) = undefined
instance RightModule Natural (Exp ord) where
  (*.) = undefined

instance Group (Exp ord) where
  (-) = subMon

instance NFData (Exp ord) where
  rnf x = seq x () -- sujeto a revision cunado se realize la diviision
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

subMon :: Exp ord -> Exp ord -> Exp ord
subMon xs xz = Exp $ on verificationMl getExp xs xz

instance Unital (Exp ord ) where
  one = Exp empty

addMon :: Exp ord -> Exp ord -> Exp ord
addMon xs xz =  Exp$ on verificationMl getExp xs xz

verificationMl :: ExpA -> ExpA -> ExpA -- Array D Ix1 Double -> Array D Ix1 Double -> Array D Ix1 Double
verificationMl xs xz
  | xs == xz = xs
  | otherwise = throw $ SizeElementsMismatchException (size xs) (size xz)

showMon :: (Num a, Eq a, Ord a, Show a) => [a] -> Int -> String
showMon [] _ = "Empty Exponents"
showMon (x:xs) s
  | x == 0 = printMon 
  | null xs = format
  | otherwise = format ++ printMon
  where
    next = succ
    format = "x_{" ++ show s ++ "}^{" ++ show x ++ "}"
    printMon = showMon xs (next s)


multMon ::  Exp ord  -> Exp ord  -> Exp ord
multMon xs xz = Exp $ on (A.liftArray2 (P.+)) getExp xs xz

divMon :: Exp ord -> Exp ord -> Exp ord
divMon xs xz = Exp $ on (A.liftArray2 (P.-)) getExp xs xz

-- * Names for orderings.
--   We didn't choose to define one single type for ordering names for the extensibility.
-- | Lexicographical order
data Lex = Lex
-- | Reverse lexicographical order
data Revlex = Revlex

instance Ord (Exp Revlex) where
  compare = on revlex' (toList . getExp)

instance Ord (Exp Lex) where
  compare = on lex' (toList . getExp)
  (>) = on (P.>) (toList . getExp)
  (<) = on (P.<) (toList . getExp)


lex' :: (Num a, Eq a, Ord a) => [a] -> [a] -> Ordering
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


