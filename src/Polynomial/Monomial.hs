
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
      MonArray,
      -- * Classes
      IsMonomialOrder,
      -- * Functions
      mon
    )
    where
import Data.Massiv.Array as A
import Data.Char.SScript
import Prelude hiding (lex)
import Data.Function

-- | MonAarray is a list of coefficient and an monomial order to use in the polynomial definition
newtype MonArray ord = MonArray {getMon :: Array P Ix1 Int} deriving (Eq)

data  Lex = Lex       -- ^Just the datatype for Lex ordering
data Revlex = Revlex -- ^Just the datatype for Revlex ordering

-- << Class Functions >>
----------------------------------------------------------------------------------------
-- | Definition of what a monomial order must meet
class IsMonomialOrder ord where
    -- | Monomial order (of degree n). This should satisfy the following methods
  compareMonArray :: MonArray ord -> MonArray ord  -> Ordering
---------------------------------------------------------------------------------------

-- <<Instances>>
instance Show (MonArray ord ) where
  show monomial = formatSS $ showMonArray ( getMon monomial) 0

instance (IsMonomialOrder ord) => Ord (MonArray ord) where
   compare = compareMonArray

instance IsMonomialOrder Lex where
  compareMonArray m n = (lex `on` (toList . getMon)) m n


instance IsMonomialOrder Revlex where
 compareMonArray m n = (revlex `on` (toList . getMon)) m n


---------------------------------------------------------------------------------------

-- <<Functions>>
-- |The 'mon' function creates a monomial since an integer array
mon :: [Int]  -> MonArray Lex
mon values = MonArray (makeVectorR P Seq (Sz $ length values) (\x -> values !! x))

-- mon version of list
-- mon ::  [[Int]]  -> MonArray ord
-- mon values = MonArray $ fromLists' Par values

---------------------------------------------------------------------------------------
showMonArray :: Array P Ix1 Int -> Int -> String
showMonArray monomial n
  | n  == elemsCount monomial = "\n" ++ show monomial
  | degree == 0 = next
  | otherwise = "x_{" ++ show n ++ "}^{" ++ show degree ++ "}" ++ next
  where
    degree  =  monomial !> n
    next = showMonArray monomial (n+1)

-- function  for the instance show (array)
-- showMonArray :: Array P Ix2 Int -> Int -> String
-- showMonArray monomial x
--   | x  == div (elemsCount monomial) 2 = "\n"
--   | degree == 0 = next
--   | otherwise = "x_{" ++ show var ++ "}^{" ++ show degree ++ "}" ++ next
--   where
--     var  =  monomial ! x :. 0
--     degree  =   monomial ! x :. 1
--     next = showMonArray monomial (x+1)
---------------------------------------------------------------------------------------

lex :: [Ix1] -> [Ix1] -> Ordering
lex [] [] = EQ
lex [] _ = LT
lex _ [] = GT
lex (x:xs) (y:ys)
    |  x==y = lex xs ys
    | x > y = GT
    | otherwise = LT
---------------------------------------------------------------------------------------
revlex :: [Ix1] -> [Ix1] -> Ordering
revlex [] [] = EQ
revlex [] _ = LT
revlex _ [] = GT
revlex x y
    | (xr == 0 && yr == 0) || xr==yr = revlex (reverse xrs) (reverse yrs)
    | xr > yr = GT
    | otherwise = LT
    where
        (xr:xrs) = reverse x
        (yr:yrs) = reverse y
