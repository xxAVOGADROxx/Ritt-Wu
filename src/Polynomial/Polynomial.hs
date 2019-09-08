{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Polynomial.Polynomial()where
import Polynomial.Terms
import Polynomial.Monomial
import Prelude as P
import Data.Massiv.Array as A
import Control.Scheduler
import Numeric.Algebra as N
import Data.Function -- on
import Data.Char.SScript
import Data.List


newtype Poly t ord =
  Poly
    { getP :: [Term t ord]
    } deriving (Eq)
-----------------------------------------------------------------------------------------------------
class (DecidableZero r, Ring r, Commutative r, Eq r) =>   CoefficientRing r
instance (DecidableZero r, Ring r, Commutative r, Eq r) => CoefficientRing r
---------------------------------------------------- << FUNCTIONS >>-----------------------------------------
--- The class is independent of the monomial order, in theory but in Revlex we take the last elemens in a monomial
class' :: Poly t ord -> Int
class' xs = max' $ P.map (elemsCount . getMon . snd . getT) (getP xs)

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
    | x <- (P.map (getMon . snd . getT) (getP xp))
    , class' xp == elemsCount x
    ]
-- LEX

--------------------------------------------------------------
-- multidegree
--mdP :: Poly t ord -> [Int]
--mdP = undefined
--------------------------------------------------------------
-- leading term
lt :: (Eq t) => Poly t Revlex -> Term t Revlex
lt xp = lmMax' $ [ x | x <- (getP xp), ld xp == f x ]
  where
    f = last . A.toList . getMon . snd . getT

lmMax' :: (Eq t )=>[Term t Revlex] -> Term t Revlex
lmMax' [x] = x
lmMax' (x:x':xs) = lmMax'
    ((if x >= x'
        then x
        else x') :
     xs)
---------------------------------------------------------------
lm :: (Num t, Eq t ) => Poly t Revlex -> Term t Revlex
lm xp =  Term (1, xs)
  where
    (x,xs) = getT $ lt xp
--------------------------------------------------------------
-- leading coefficient in x_p (class f = p), is all the terms without the leading variable (the initial) --ojo el inicial debe estar fact?
lc :: (Eq t) => Poly t Revlex -> Term t Revlex
lc xp = lc' $ getT $ lt xp

lc' :: (k, Mon Revlex ) -> Term k Revlex
lc' (s, xs) = Term $ (s,  m . init . toList . getMon $ xs)
--------------------------------------------------------------
-- reduction of the leading monomial
red :: (Eq t) => Poly t Revlex -> Poly t Revlex
red xs = Poly [x | x <- (getP xs), x /= a]
  where
    a = lt xs
--------------------------------------------------------------
--Monomial exponentation
expM :: (Num k) => Term k ord -> Int -> Term k ord
expM m i = Term (p, Mon $ A.map (P.*i) b)
  where
    (k,m') = getT m
    p = foldl (P.*) 1 $ take i $ cycle [k]
    b  = getMon m'
--------------------------------------------------------------
--- comparison total degree (muti degree monomial)
mdM :: Term k ord -> [Int]
mdM = toList . getMon . snd .  getT

--------------------------------------------------------------
lcmT ::  (Ord t, Num t) => Term t Revlex -> Term t Revlex  -> Term t Revlex
lcmT a b =  on lcm' (getT ) a b

lcm' :: (Ord t )=> (t ,Mon Revlex) ->  (t, Mon Revlex) -> Term t Revlex
lcm' (a, b) (c, d) = Term (max a c, m $ P.zipWith  (max) (toList . getMon  $  b) (toList . getMon  $ d))
--------------------------------------------------------------
mon :: Term t ord -> Mon ord 
mon m = Mon $ getMon b
  where
    (a,b) = getT m
--------------------------------------------------------------
-- spoly :: (Fractional t, Num t, Eq t, Ord t )=> Poly t Revlex -> Poly t Revlex -> Poly t Revlex
-- spoly f g = Poly [(x' N./ lm f )] N.* f N.- Poly [(x' N./ lm g)] N.* g
--   where
--     x' = lcmT f g

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

showPoly :: (Ord t, Num t, Show t) => [Term t ord] -> String
showPoly [] = ""
showPoly (x:xs)
  | (fst $ getT x) P.> 0 = " + " ++ show x ++ showPoly xs
  | otherwise = show x ++ showPoly xs

---------------------------------------------------------------
instance (Num t, Eq t) => Additive (Poly t Revlex) where
  (+) a b = Poly $ P.map h $ groupBy (\(a,b)(c,d) -> (==) b d) $ sortBy (\(a,b)(c,d) -> compare b d) (P.map getT $ on (++) getP a b )

h :: (Num k)=>  [(k, Mon ord)] -> Term k ord
h [] = zero
h [x] = Term x
h (x:xs) = (Term x) N.+ h xs

addPol :: (Num t) =>[Term t ord ] -> [Term t ord] -> [Term t ord]
addPol xs xp = concat [ maybe [] id (maybeMon x y) |  x <- xs, y <- xp ]

maybeMon :: (Num t ) => Term t ord -> Term t ord -> Maybe ([Term t ord])
maybeMon a b
  | mon a == mon b = Just $ [a N.+ b]
  | otherwise = Nothing

-- rmdups :: Ord t => [Term t ord] -> [Term t ord]
-- rmdups = rmdups' Set.empty where
--   rmdups' _ [] = []
--   rmdups' a (b : c) = if Set.member b a
--     then rmdups' a c
--     else b : rmdups' (Set.insert b a) c
----------------------------------------------------------------------------

instance (Num t, Eq t) => Semiring (Poly t Revlex)
instance (Num t, Eq t) => Abelian (Poly t Revlex)
instance (Num t) => Multiplicative (Poly t ord) where
  (*) xs xp = Poly $ simplification $[ x N.* y | x <- (getP xs), y <- (getP xp) ]

simplification :: [Term t ord] -> [Term t ord]
simplification (x:xs) = undefined

-- λ> c = Poly [Term(3,m[1,2]), Term(5,m[2,3])] :: Poly Int Lex
-- λ> d = Poly [Term(2,m[2,1]), Term(2,m[3,5])] :: Poly Int Lex
-- λ> c N.* d
-- +6x₁³x₂³+6x₁⁴x₂⁷+10x₁⁴x₂⁴+10x₁⁵x₂⁸
-- λ> 

instance (Num t, Eq t) => Group (Poly t Revlex) where
  (-) =  undefined --

instance (Num t, Eq t) => LeftModule Integer (Poly t Revlex) where
  (.*) = undefined
instance (Num t, Eq t) => RightModule Integer (Poly t Revlex) where
  (*.) = undefined
instance (Num t, Eq t) => LeftModule Natural (Poly t Revlex) where
  (.*) = undefined
instance (Num t, Eq t) => RightModule Natural (Poly t Revlex) where
  (*.) = undefined

instance (Num t, Eq t) => Monoidal (Poly t Revlex) where
  zero = Poly []

-- a =  Poly [Term(1,m[1,2]), Term(2,m[3,4]), Term(4,m[1,2])] :: Poly Int Lex
-- sortBy (\ (Term (_,b)) (Term(_,d)) -> compare b d) $ getP a

-- poly = sum of terms
-- temr = coeff * monomio
-- monomio = set of univariantes
-- lm = highest value in the monomial 
-- f = Poly [Term (1,m[1,2]), Term(1, m[0])] :: Poly Int Revlex
-- g = Poly [Term (2,mp[2][3]), Term(-1, mp[2][2]), Term(1,m[2,1])] :: Poly Int Revlex

scheduleSums :: IO [Int]
scheduleSums =
  withScheduler (ParOn [1..4]) $ \ scheduler -> do
    scheduleWork scheduler $ pure (10 P.+ 1)
    scheduleWork scheduler $ pure (20 P.+ 2)
    scheduleWork scheduler $ pure (30 P.+ 3)
    scheduleWork scheduler $ pure (40 P.+ 4)
    scheduleWork scheduler $ pure (50 P.+ 5)

--  Poly [Term(4,m[1,2]), Term(-3,m[5,6]), Term(6,mp[4][9])] :: Poly Int Lex
-- Poly [Term(4,m[1,2]), Term(-3,m[5,10]), Term(6,mp[4][9]), Term(15, mp[1,3][3,9]), Term(30, mp[3,4,5][1,2,3]), Term(34, mp[3,4,5][2,2,3])] :: Poly Int Lex
-- a = Poly [Term(4,m[1,2]), Term(-3,m[5,10]), Term(6,mp[4][9]), Term(15, mp[1,3][3,9]), Term(30, mp[2,3,4,5][4,1,2,3]), Term(34, mp[2,3,4,5][8,2,2,3])] :: Poly Int Lex

-- f = Poly [Term(1,m[2,3]), Term(-1,m[0,1])] :: Poly Int Lex
-- g = Poly [Term(1,m[3,1]), Term(-2,m[])] :: Poly Int Lex

-- f :: Int -> Maybe Int
-- f x
--   | x P.> 1 = Just x
--   | otherwise = Nothing

-- a = Poly [Term(1,m[2,1]), Term(1,m[1,2])] :: Poly Int Revlex
-- b = Poly [Term(1,m[3]), Term(2,m[2,1]), Term(1,mp[3][1]), Term(3,m[1,2])] :: Poly Int Revlex
-- c = on (++) getP a b
-- f = groupBy (\(a,b)(c,d) -> compare b d) $ sortBy (\(a,b)(c,d) -> compare b d) (P.map getT c)
-- g = P.map h $ groupBy (\(a,b)(c,d) -> (==) b d) $ sortBy (\(a,b)(c,d) -> compare b d) (P.map getT c)

