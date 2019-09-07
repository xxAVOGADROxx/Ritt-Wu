{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Polynomial.Polynomial()where
import Polynomial.Exponent
import Polynomial.Monomial
import Prelude as P
import Data.Massiv.Array as A
import Control.Scheduler
import Numeric.Algebra as N
--import Data.List
import Data.Function -- on
import Data.Char.SScript

newtype Poly t ord =
  Poly
    { getP :: [Mon t ord]
    } deriving (Eq)

---------------------------------------------------- << FUNCTIONS >>-----------------------------------------
--- The class is independent of the monomial order, in theory but in Revlex we take the last elemens in a monomial
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
-- leading variable or leading term
lv :: Poly t Revlex -> String
lv p =  "x_{" ++ show (class' p)  ++ "}"
--  LEX
--lv :: Poly t Lex -> String
--lv p =  "x_{" ++ show ( p)  ++ "}"
--------------------------------------------------------------
-- leading degreee
ld :: Poly t Revlex -> Int
ld xp =
  max' $
  P.map
    last
    [ A.toList x
    | x <- (P.map (getExp . snd . getMon) (getP xp))
    , class' xp == elemsCount x
    ]
-- LEX

--------------------------------------------------------------
-- multidegree
--mdP :: Poly t ord -> [Int]
--mdP = undefined
--------------------------------------------------------------
-- leading monomial, completo monomial
lm :: (Eq t) => Poly t Revlex -> Mon t Revlex
lm xp = lmMax' $ [ x | x <- (getP xp), ld xp == f x ]
  where
    f = last . A.toList . getExp . snd . getMon

lmMax' :: (Eq t )=>[Mon t Revlex] -> Mon t Revlex
lmMax' [x] = x
lmMax' (x:x':xs) = lmMax'
    ((if x >= x'
        then x
        else x') :
     xs)

--------------------------------------------------------------
-- leading coefficient in x_p (class f = p), is all the terms without the leading variable (the initial) --ojo el inicial debe estar fact?
lc :: (Eq t) => Poly t Revlex -> Mon t Revlex
lc xp = lc' $ getMon $ lm xp

lc' :: (k, Exp Revlex ) -> Mon k Revlex
lc' (s, xs) = Mon $ (s,  m . init . toList . getExp $ xs)
--------------------------------------------------------------
-- reduction of the leading monomial
red :: (Eq t) => Poly t Revlex -> Poly t Revlex
red xs = Poly [x | x <- (getP xs), x /= a]
  where
    a = lm xs
--------------------------------------------------------------
--Monomial exponentation
expM :: (Num k) => Mon k ord -> Int -> Mon k ord
expM m i = Mon (p, Exp $ A.map (P.*i) b)
  where
    (k,m') = getMon m
    p = foldl (P.*) 1 $ take i $ cycle [k]
    b  = getExp m'
--------------------------------------------------------------
--- comparison total degree (muti degree monomial)
mdM :: Mon k ord -> [Int]
mdM = toList . getExp . snd .  getMon

--------------------------------------------------------------
lcmA ::  (Ord t) => Poly t Revlex -> Poly t Revlex  -> Mon t Revlex
lcmA a b =  on lcm' (getMon . lm) a b

lcm' :: (Ord t )=>(t,Exp Revlex) ->  (t, Exp Revlex) -> Mon t Revlex
lcm' (a, b) (c, d) = Mon (max a c, m $ P.zipWith  (max) (toList . getExp  $  b) (toList . getExp  $ d))
--------------------------------------------------------------
--------------------------------------------------------------
--spoly :: Poly t Revlex -> Poly t ord -> Poly t Revlex
--spoly f g = 

-- pseudo remainder
-- prem :: (Eq t, Num t) => Poly t Lex -> Poly t Lex -> Int
-- prem g' f'
--   | h <= r && r' /= Poly [] = aux 
--   | otherwise = l' r'
--   where
--     r' = g'
--     r = ld r'
--     h' = f'
--     h = ld h'
--     d = r-h +1
--     l' = lc h'

-------------------------------------- <<INSTANCES >> ------------------------------------
instance (Ord t, Num t, Show t) => Show (Poly t ord) where
  show m = showPoly (getP m)

showPoly :: (Ord t, Num t, Show t) => [Mon t ord] -> String
showPoly [] = ""
showPoly (x:xs)
  | (fst $ getMon x) P.> 0 = "+" ++ show x ++ showPoly xs
  | otherwise = show x ++ showPoly xs

---------------------------------------------------------------
-- instance (Num t) => Additive (Poly t Lex) where
--   (+) =  undefined
--addPolys :: Poly t ord -> Poly t ord -> Maybe (Poly t ord )
--addPolys xs xp = Poly [ x N.+ y | x <- getP xs, y <- getP xp , ]

scheduleSums :: IO [Int]
scheduleSums =
  withScheduler (ParOn [1..4]) $ \ scheduler -> do
    scheduleWork scheduler $ pure (10 P.+ 1)
    scheduleWork scheduler $ pure (20 P.+ 2)
    scheduleWork scheduler $ pure (30 P.+ 3)
    scheduleWork scheduler $ pure (40 P.+ 4)
    scheduleWork scheduler $ pure (50 P.+ 5)

--  Poly [Mon(4,m[1,2]), Mon(-3,m[5,6]), Mon(6,mp[4][9])] :: Poly Int Lex
-- Poly [Mon(4,m[1,2]), Mon(-3,m[5,10]), Mon(6,mp[4][9]), Mon(15, mp[1,3][3,9]), Mon(30, mp[3,4,5][1,2,3]), Mon(34, mp[3,4,5][2,2,3])] :: Poly Int Lex
-- a = Poly [Mon(4,m[1,2]), Mon(-3,m[5,10]), Mon(6,mp[4][9]), Mon(15, mp[1,3][3,9]), Mon(30, mp[2,3,4,5][4,1,2,3]), Mon(34, mp[2,3,4,5][8,2,2,3])] :: Poly Int Lex

-- f = Poly [Mon(1,m[2,3]), Mon(-1,m[0,1])] :: Poly Int Lex
-- g = Poly [Mon(1,m[3,1]), Mon(-2,m[])] :: Poly Int Lex

-- f :: Int -> Maybe Int
-- f x
--   | x P.> 1 = Just x
--   | otherwise = Nothing


instance (Num t) => Semiring (Poly t ord)
instance (Num t) => Abelian (Poly t ord)
instance (Num t) => Multiplicative (Poly t ord) where
  (*) xs xp = Poly $ simplification $[ x N.* y | x <- (getP xs), y <- (getP xp) ]

simplification :: [Mon t ord] -> [Mon t ord]
simplification (x:xs) = undefined

-- λ> c = Poly [Mon(3,m[1,2]), Mon(5,m[2,3])] :: Poly Int Lex
-- λ> d = Poly [Mon(2,m[2,1]), Mon(2,m[3,5])] :: Poly Int Lex
-- λ> c N.* d
-- +6x₁³x₂³+6x₁⁴x₂⁷+10x₁⁴x₂⁴+10x₁⁵x₂⁸
-- λ> 

instance Additive (Poly t ord) where
  (+) = undefined
instance (Num t) => LeftModule Natural (Poly t ord) where
  (.*) = undefined
instance (Num t) => RightModule Natural (Poly t ord) where
  (*.) = undefined

instance (Num t) => Monoidal (Poly t ord) where
  zero = Poly []

-- a =  Poly [Mon(1,m[1,2]), Mon(2,m[3,4]), Mon(4,m[1,2])] :: Poly Int Lex
-- sortBy (\ (Mon (_,b)) (Mon(_,d)) -> compare b d) $ getP a

-- poly = sum of terms
-- temr = coeff * monomio
-- monomio = set of univariantes
-- lm = highest value in the monomial 
-- f = Poly [Mon (1,m[1,2]), Mon(1, m[])] :: Poly Int Revlex
-- g = Poly [Mon (2,mp[2][3]), Mon(-1, mp[2][2]), Mon(1,m[2,1])] :: Poly Int Revlex
