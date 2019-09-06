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
module Polynomial.Exponent
  (
   -- * Types
   Exp(..),
   ExpA(..),

   Lex(..),
   Revlex(..),
    -- * Classes

    -- * Functions
   NFData(..),
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
--import GHC.Generics (Generic, Generic1)


-- | Array monomial with representation r
type ExpA   = Array D Ix1 Int
-- The above representation need to know the representation (r) of array.

-- | A wrapper for monomials with a certain (monomial) order
newtype Exp ord =
  Exp
    { getExp :: ExpA
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
m :: [Int] -> Exp ord
m xs = Exp $ makeVectorR D Par (Sz $ length xs) (xs !!)
-----------------------------------------------------------------------------------------
-- Function that recive the x_i position with the corresponding exp
-- | Monomial with the term position
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
  | p == 0 = error "No se puede crear en la posicion 0, sugerido 1"
  | n /= p = 0 : mp' (p : ps) (x : xs) next
  | otherwise = x : mp' (ps) (xs) next
  where
    next = n P.+ 1
-----------------------------------------------------------------------------------------
------- <<INSTANCES >>--------------
instance  Show (Exp ord) where
  show m = formatSS $ showMon (A.toList (getExp m)) 1

showMon :: (Num a, Eq a, Ord a, Show a) => [a] -> Int -> String
showMon [] _ = "Empty Term"
showMon (x:xs) s
  | x == 0 = printMon
  | null xs = format
  | otherwise = format ++ printMon
  where
    next = succ
    format = "x_{" ++ show s ++ "}^{" ++ show x ++ "}"
    printMon = showMon xs (next s)
---------------------------------------------------------------------------------------
instance Multiplicative (Exp ord) where
  (*) xs xz = Exp $ on (A.liftArray2 (P.+)) getExp xs xz
---------------------------------------------------------------------------------------
instance Division (Exp ord) where
  (/) xs xz = Exp $ on (A.liftArray2 (P.-)) getExp xs xz
---------------------------------------------------------------------------------------
instance Additive (Exp ord) where
  (+) xs xz = Exp $ on (verificationMl) getExp xs xz
---------------------------------------------------------------------------------------
instance Semiring (Exp ord)
instance Abelian (Exp ord)
instance Monoidal (Exp ord) where
  zero = Exp empty
instance LeftModule Integer (Exp ord) where
  (.*) = undefined
instance RightModule Integer (Exp ord) where
  (*.) = undefined
instance LeftModule Natural (Exp ord) where
  (.*) = undefined
instance RightModule Natural (Exp ord) where
  (*.) = undefined
--------------------------------------------------------------------------------------- 
instance Group (Exp ord) where
  (-)  xs xz = Exp $ on verificationMl getExp xs xz

verificationMl :: ExpA -> ExpA -> ExpA
verificationMl xs xz
  | xs == xz = xs
  | otherwise = error "The size doesn't match" 
---------------------------------------------------------------------------------------
instance NFData (Exp ord) where
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
---------------------------------------------------------------------------------------
instance Ord (Exp Revlex) where
  compare = on revlex' (toList . getExp)

revlex' :: (Num a, Eq a, Ord a) => [a] -> [a] -> Ordering
revlex' = on lex' reverse
---------------------------------------------------------------------------------------
instance Unital (Exp ord ) where
  one = undefined --Exp empty
-- λ> a = Exp empty
-- λ> getExp a
-- Array D Seq (Sz1 0)
--   [  ]
-- λ>






