{-# LANGUAGE FlexibleInstances #-}
module Polynomial.Polynomial()where
import Polynomial.Exponent
import Polynomial.Monomial
import Prelude as P
import Data.Massiv.Array as A
import Control.Scheduler

newtype Poly t ord =
  Poly
    { getP :: [Mon t ord]
    }

instance (Ord t, Num t, Show t) => Show (Poly t Lex) where
  show m = showPoly (getP m)

showPoly :: (Ord t, Num t, Show t) => [Mon t Lex] -> String
showPoly [] = ""
showPoly (x:xs)
  | (fst $ getMon x) > 0 = "+" ++ show x ++ showPoly xs
  | otherwise = show x ++ showPoly xs
---------------------------------------------------------------
class' :: Poly t ord -> Int
class' xs = max' $ P.map (elemsCount . getExp . snd . getMon) (getP xs)

max' :: [Int] -> Int
max' [x] = x
max' (x:x':xs) =
  max'
    ((if x >= x'
        then x
        else x') :
     xs)

--------------------------------------------------------------
-- leading variable
lv :: Poly t ord -> Int
lv = class'
--------------------------------------------------------------
-- leading degreee
ld :: Poly t ord -> Int
ld xp =
  max' $
  P.map
    last
    [ A.toList x
    | x <- (P.map (getExp . snd . getMon) (getP xp))
    , class' xp == elemsCount x
    ]
--------------------------------------------------------------
-- multidegree
md :: Poly t ord -> [Int]
md = undefined

--------------------------------------------------------------
-- leading monomial
lt :: (Eq t) => Poly t Lex -> Mon t Lex
lt xp = ltMax' $ [ x | x <- (getP xp), ld xp == f x ]
  where
    f = last . A.toList . getExp . snd . getMon

ltMax' :: (Eq t )=>[Mon t Lex] -> Mon t Lex
ltMax' [x] = x
ltMax' (x:x':xs) = ltMax'
    ((if x >= x'
        then x
        else x') :
     xs)

--------------------------------------------------------------
-- leading coefficient
lc :: (Eq t) => Poly t Lex -> t
lc xp = fst (getMon $ lt xp)
--------------------------------------------------------------
-- leading monomial
lm :: (Eq t) => Poly t Lex -> Exp Lex
lm xp = snd (getMon $ lt xp)
--------------------------------------------------------------
-- pseudo remainder
srem :: Poly t ord -> Poly t ord -> Poly t ord
srem =  undefined



scheduleSums :: IO [Int]
scheduleSums =
  withScheduler (ParOn [1..4]) $ \ scheduler -> do
    scheduleWork scheduler $ pure (10 + 1)
    scheduleWork scheduler $ pure (20 + 2)
    scheduleWork scheduler $ pure (30 + 3)
    scheduleWork scheduler $ pure (40 + 4)
    scheduleWork scheduler $ pure (50 + 5)

--  Poly [Mon(4,m[1,2]), Mon(-3,m[5,6]), Mon(6,mp[4][9])] :: Poly Int Lex
-- Poly [Mon(4,m[1,2]), Mon(-3,m[5,10]), Mon(6,mp[4][9]), Mon(15, mp[1,3][3,9]), Mon(30, mp[3,4,5][1,2,3]), Mon(34, mp[3,4,5][2,2,3])] :: Poly Int Lex
-- a = Poly [Mon(4,m[1,2]), Mon(-3,m[5,10]), Mon(6,mp[4][9]), Mon(15, mp[1,3][3,9]), Mon(30, mp[2,3,4,5][4,1,2,3]), Mon(34, mp[2,3,4,5][8,2,2,3])] :: Poly Int Lex
