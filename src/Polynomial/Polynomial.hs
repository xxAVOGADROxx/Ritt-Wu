{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Polynomial.Polynomial
  (
    Poly(..),
    class',
    ld,
    prem,
    lc,
    lt,
    lm,
    initOfv,
    max',
    spoly
  ) where
import Polynomial.Terms
import Polynomial.Monomial
import Prelude as P
import Data.Massiv.Array as A
import Control.Scheduler
import Numeric.Algebra as N
import Data.Function -- on
import Data.Char.SScript
import Data.List  as L
--import Data.Set
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
lv :: Poly t Revlex -> Int
lv p =  class' p
--  LEX
--lv :: Poly t Lex -> String
--lv p =  "x_{" ++ show ( p)  ++ "}"
--------------------------------------------------------------
-- leading degreee
ld :: Poly t ord -> Int
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
--MULTIVARIABLE -------------------------------------------------
-- ---------------------------------------------------------------
lm :: (Num t, Eq t ) => Poly t Revlex -> Term t Revlex
lm xp =  Term (1, xs)
  where
    (x,xs) = getT $ lt xp
-- --------------------------------------------------------------
-- -- Initial of a palynomial with respect to a variable 
initOfv :: (Eq t) => Poly t Revlex -> Poly t Revlex
initOfv xp = Poly [ g x | x <- getP xp, f x == ld xp && class' xp == h x]
  where
    f = last . toList . getMon . snd . getT
    g = takeInit
    h =  elemsCount . getMon . snd . getT

initOfv' :: (Eq t) => Poly t Revlex -> Poly t Revlex
initOfv' xp = Poly [x | x <- getP xp, f x == ld xp && class' xp == h x]
  where
    f = last . toList . getMon . snd . getT
    h =  elemsCount . getMon . snd . getT

takeInit :: Term t ord -> Term t ord
takeInit x = Term (a, m $ f b)
  where
    (a,b) = getT x
    f = init . toList . getMon
--------------------------------------------------------------

lc :: (Num t, Eq t ) => Poly t Revlex -> Term t Revlex
lc xp =  Term (x, m[0])
  where
    (x,xs) = getT $ lt xp
--------------------------------------------------------------
-- reduction of the leading monomial
red :: (Eq t) => Poly t Revlex -> Poly t Revlex
red xs = Poly [x | x <- (getP xs), x /= a]
  where
    a = lt xs

------------------------------------------------------------
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
-- lcm para polynomios
lcmP ::  (Ord t, Num t, Integral t) => Poly t Revlex -> Poly t Revlex -> Poly t Revlex
lcmP xs xp
  | length (getP xs) == 1 && length (getP xp) == 1 = Poly[ on lcmT (head . getP) xs xp]
  | length (getP xp) == 1 =   (withoutFactor xs)  N.* Poly [lcmT (head . getP $ xp) (factor xs)]
  | otherwise = error "completar esta seccion"

factor :: (Num t) => Poly t Revlex -> Term t Revlex
factor ps = Term (1, m $ commList)
  where
    --k = mcd [ a | x <- getP ps, let (a,b) = getT x]
    commList = L.foldl1 (P.zipWith min) [(toList . getMon) b | x <- getP ps, let (a,b) = getT x]

withoutFactor :: (Integral t) => Poly t Revlex -> Poly t Revlex
withoutFactor xs = Poly [ Term (a', b' N.- b) | x <- getP xs, let (a',b') = getT x]
  where (a,b) = getT $ factor xs



lcm'' :: (Ord t, Num t )=> [Term t Revlex] -> [Term t Revlex] -> [Term  t Revlex]
lcm'' xs xp
  | length xs == 1 && length xp == 1 = [ on lcmT (head) xs xp]
  | otherwise = undefined

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
-- spolynomial
basicSpoly :: (Integral t, Fractional t, Num t, Ord t )=> Poly t Revlex -> Poly t Revlex -> Poly t Revlex
basicSpoly f g = (basicSpoly x'  (initOfv' f )) N.* f N.- (basicSpoly x' (initOfv' g)) N.* g --initOFv' ? instead of lt YES
  where
    x' = on lcmP initOfv' f g
----------------------------------------------------------------
spoly :: (Integral t, Fractional t, Ord t ) => Poly t Revlex -> Poly t Revlex -> Poly t Revlex
spoly f g
  | ld f >= ld g && class' g == class' f  = basicSpoly ( spoly f g) g
  | otherwise = f
------------------------------------------------------------
-- pseudo remainder
prem :: (Eq t, Num t) => Poly t Revlex -> Poly t Revlex -> Poly t Revlex
prem g f
  | g /= Poly[] && class' g == class' f && ld g >= m = prem (calc)(f)
  | otherwise = g
  where
    m = ld f
    calc = (initOfv f N.* g) N.- (initOfv g N.* f N.* Poly [Term(1,mp[lv f][ld g P.- m])])
----------------------- <<INSTANCES >> ------------------------------------
instance (Ord t, Num t, Show t) => Show (Poly t ord) where
  show m = showPoly (getP m)

showPoly :: (Ord t, Num t, Show t) => [Term t ord] -> String
showPoly [] = ""
showPoly (x:xs)
  | (fst $ getT x) P.> 0 = " + " ++ show x ++ showPoly xs
  | (fst $ getT x) P.< 0 = " - " ++ show x ++ showPoly xs
  | otherwise = show x ++ showPoly xs

---------------------------------------------------------------
instance (Num t, Eq t) => Additive (Poly t Revlex) where
  (+) a b =
    Poly $
    filter removeZero $
    P.map tsum $
    groupBy (\(a, b) (c, d) -> (==) b d) $
    sortBy (\(a, b) (c, d) -> compare b d) (P.map getT $ on (++) getP a b)

tsum :: (Num t) => [(t, Mon ord)] -> Term t ord
tsum [x] = Term x
tsum (x:xs) = (Term x) N.+ tsum xs

removeZero :: (Num t, Eq t) => Term t ord -> Bool
removeZero term
  | a /= 0 = True
  | otherwise = False
  where
    (a,b) = getT term
-- addPol :: (Num t) => [Term t ord] -> [Term t ord] -> [Term t ord]
-- addPol xs xp = concat [maybe [] id (maybeMon x y) | x <- xs, y <- xp]

-- maybeMon :: (Num t) => Term t ord -> Term t ord -> Maybe ([Term t ord])
-- maybeMon a b
--   | mon a == mon b = Just $ [a N.+ b]
--   | otherwise = Nothing

-- rmdups :: Ord t => [Term t ord] -> [Term t ord]
-- rmdups = rmdups' Set.empty where
--   rmdups' _ [] = []
--   rmdups' a (b : c) = if Set.member b a
--     then rmdups' a c
--     else b : rmdups' (Set.insert b a) c
----------------------------------------------------------------------------

instance (Num t, Eq t) => Semiring (Poly t Revlex)
instance (Num t, Eq t) => Abelian (Poly t Revlex)
instance (Num t, Eq t) => Multiplicative (Poly t Revlex) where
  (*) xs xp = simp $ Poly $ [ x N.* y | x <- (getP xs), y <- (getP xp) ]

simp :: (Num t, Eq t) => Poly t Revlex -> Poly t Revlex 
simp xs =
    Poly $
    filter removeZero $
    P.map tsum $
    groupBy (\(a, b) (c, d) -> (==) b d) $
    sortBy (\(a, b) (c, d) -> compare b d) (P.map getT $ getP xs )
---------------------------------------------------------------------------
-- instance Division (Mon ord) where
--   (/) xs xz =

-- λ> c = Poly [Term(3,m[1,2]), Term(5,m[2,3])] :: Poly Int Lex
-- λ> d = Poly [Term(2,m[2,1]), Term(2,m[3,5])] :: Poly Int Lex
-- λ> c N.* d
-- +6x₁³x₂³+6x₁⁴x₂⁷+10x₁⁴x₂⁴+10x₁⁵x₂⁸
-- λ>
-- 22 marzo bryan

instance (Num t, Eq t) => Group (Poly t Revlex) where
  (-) a b =
    Poly $
    filter removeZero $
    P.map tsum $
    groupBy (\(a, b) (c, d) -> (==) b d) $
    sortBy (\(a, b) (c, d) -> compare b d) (P.map getT $  (++) (getP a) (change $ getP b)  )

change :: (Num t) => [Term t ord] -> [Term t ord]
change [] = []
change (x:xs) = Term ((-1) P.* a, b) : change xs
   where
    (a,b) = getT x


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

--------------------------FACTOR-----------------
prime_factors :: Int -> [Int]

prime_factors 1 = []
prime_factors n
  | factors == []  = [n]
  | otherwise = factors ++ prime_factors (n `div` (head factors))
  where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n P.- 1]

mcd' :: [Int] -> [[Int]]
mcd' ps = P.map (prime_factors) ps

mcd :: [Int] -> Int
mcd ps =  P.foldl1 (P.*) $ P.foldl1 mezc' (mcd' ps)

mezc' :: [Int] -> [Int] -> [Int]
mezc' [] _ = []
mezc' _ [] =  []
mezc' (x:xs)(y:ys)
  | x P.< y       = mezc' xs (y:ys)
  | x P.> y       = mezc'(x:xs) ys
  | x == y      = [x] ++ mezc' xs ys
------------------------------------------------

--------------------------FACTOR-----------------
-- a =  Poly [Term(1,m[1,2]), Term(2,m[3,4]), Term(4,m[1,2])] :: Poly Int Lex
-- sortBy (\ (Term (_,b)) (Term(_,d)) -> compare b d) $ getP a

-- poly = sum of terms
-- temr = coeff * monomio
-- monomio = set of univariantes
-- lm = highest value in the monomial 
-- f = Poly [Term (1,m[1,2]), Term(1, m[0])] :: Poly Int Revlex
-- g = Poly [Term (2,mp[2][3]), Term(-1, mp[2][2]), Term(1,m[2,1])] :: Poly Int Revlex

--  Poly [Term(4,m[1,2]), Term(-3,m[5,6]), Term(6,mp[4][9])] :: Poly Int Lex
-- Poly [Term(4,m[1,2]), Term(-3,m[5,10]), Term(6,mp[4][9]), Term(15, mp[1,3][3,9]), Term(30, mp[3,4,5][1,2,3]), Term(34, mp[3,4,5][2,2,3])] :: Poly Int Lex
-- a = Poly [Term(4,m[1,2]), Term(-3,m[5,10]), Term(6,mp[4][9]), Term(15, mp[1,3][3,9]), Term(30, mp[2,3,4,5][4,1,2,3]), Term(34, mp[2,3,4,5][8,2,2,3])] :: Poly Int Lex
-- c = Poly [Term(2,m[2,1]), Term(1, m[1,2])] :: Poly Int Revlex
-- f = Poly [Term(1,m[2,3]), Term(-1,m[0,1])] :: Poly Int Lex
-- g = Poly [Term(1,m[3,1]), Term(-2,m[])] :: Poly Int Lex

-- a = Poly [Term(1,m[2,1]), Term(1,m[1,2])] :: Poly Int Revlex
-- b = Poly [Term(1,m[3]), Term(2,m[2,1]), Term(1,mp[3][1]), Term(3,m[1,2])] :: Poly Int Revlex
-- c = Poly [Term(2,m[2,1]), Term(1, m[1,2])] :: Poly Int Revlex
--Ideals
-- >>>
-- f = Poly [Term(1,m[2,3]), Term(-1, mp[2][1])] :: Poly Int Revlex
-- g = Poly [Term(1,m[3,1]), Term(-2,m[0])]  :: Poly Int Revlex
-- q = Poly [Term(1,m[8,2]), Term(2,m[5,1]), Term(4,m[2]), Term(-1, m[6])] :: Poly Int Revlex
-- r = Poly [Term(8,m[2]),Term(-2,m[6])] :: Poly Int Revlex
-- a = Poly [expM (Term(1,m[3]) :: Term Int Revlex) 3]
-- example spoly (int) Ans: (-15) % 2 + 10 % 1x₁¹(-5) % 2x₁²
-- f1 = Poly [Term(2,mp[2][2]),Term(-4,mp[2][1]), Term(1,m[2]), Term(-4,m[1]), Term(3,m[0])] :: Poly Rational Revlex
-- f2 = Poly [Term(1,mp[2][2]), Term(-2,mp[2][1]),Term(3,m[2]),Term(-12,m[1]), Term(9,m[0])] :: Poly Rational Revlex
-- example Cox 350 Ans:
-- f1 = Poly [Term (1,m[2,3]), Term(-1,mp[2][1])] :: Poly Rational Revlex
-- f2 = Poly [Term (1,m[3,1]), Term(-2,m[0])] :: Poly Rational Revlex
-- repect to y
-- f1 = Poly [Term (1,m[3,2]), Term(-1,mp[1][1])] :: Poly Rational Revlex
-- f2 = Poly [Term (1,m[1,3]), Term(-2,m[0])] :: Poly Rational Revlex
-- r f1 f2 = 8 -2x⁴
-- poly to taste the initial term
-- f1 = Poly [Term(4,m[2,1]),Term(-1,m[6,1])] :: Poly Int Revlex
-- buchberger example 19
-- f1 = Poly [Term(1,m[1,2]), Term(1,m[0])] :: Poly Int Revlex
-- f2 = Poly [Term(2,mp[2][3]), Term(-1,mp[2][2]), Term(1,m[2,1])] :: Poly Int Revlex
-- dividendo primero divisor despues
-- BUCH
-- p1 = Poly [Term(1,mp[1,4][1,1]), Term(1,mp[3][1]), Term(1,m[1,1])] :: Poly Rational Revlex
-- p2 = Poly [Term(2,mp[4][2]), Term(-2,mp[3,4][1,1]), Term(5,mp[1,2,4][1,1,1]), Term(-5,m[1,1,1])] :: Poly Rational Revlex
-- p3 = Poly [Term(1,m[1,3]), Term(-2,m[2,2]), Term(1,mp[2][1])] :: Poly Rational Revlex
-- p4 = Poly [Term(3,mp[2][4]), Term(-1,m[1])] :: Poly Rational Revlex
