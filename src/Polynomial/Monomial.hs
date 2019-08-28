
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
   -- OrderMon(..),
   --  -- * Classes
   -- MonomialOrder,
   --  -- * Functions
   -- makeMon,
   -- multMon
 )
where
import Data.Massiv.Array as A
import Prelude as P
import Data.Char.SScript
import Numeric.Algebra as N
import Data.Function
import Criterion.Main


-- | array monomial with representation r
type Monomial   = Array D Ix1 Double
-- The above representation need to know the representation (r) of array.

-- | A wrapper for monomials with a certain (monomial ) order
newtype OrderedMonomial ord  =
  OrderedMonomial
    { getMon :: Monomial
    }


makeMon :: [Double] -> OrderedMonomial ord
makeMon xs = OrderedMonomial $ makeVectorR D Par (Sz $ length xs) (xs !!)

totalDegree ::   OrderedMonomial ord  -> Double
totalDegree = (A.sum . getMon )
{-# INLINE totalDegree #-}


instance  Show (OrderedMonomial ord) where
  show m = formatSS $ showMon (toList (getMon m)) 0

instance  Multiplicative (OrderedMonomial ord ) where
 (*) = multMon

instance Division (OrderedMonomial ord) where
  (/) = divMon

instance Unital (OrderedMonomial ord ) where
  one = OrderedMonomial $ empty 


showMon :: [Double] -> Int -> String
showMon (x:xs) s
  | null xs = format
  | otherwise = format ++ printMon
  where
    next = succ
    format = "x_{" ++ show s ++ "}^{" ++ show x ++ "}"
    printMon = showMon xs (next s)


multMon ::  OrderedMonomial ord  -> OrderedMonomial ord  -> OrderedMonomial ord
multMon xs xz = OrderedMonomial $ on (A.liftArray2 (P.+))(getMon) xs xz

divMon ::  OrderedMonomial ord  -> OrderedMonomial ord  -> OrderedMonomial ord
divMon xs xz = OrderedMonomial $ on (A.liftArray2 (P.-))(getMon) xs xz

-- * Names for orderings.
--   We didn't choose to define one single type for ordering names for the extensibility.
-- | Lexicographical order
data Lex = Lex
           deriving (Show, Eq, Ord)

-- | Reversed lexicographical order
data Revlex = Revlex
              deriving (Show, Eq, Ord)
