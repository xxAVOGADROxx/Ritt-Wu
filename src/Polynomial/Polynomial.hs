{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE MultiWayIf #-}

module Polynomial.Polynomial
  (
    Poly(..),
    class',
    ld,
    prem,
    -- lc,
    -- lt,
    -- lm,
    lv,
    initOfv,
    initOfv',
    max',
    spoly,
    basicSpoly,
    lcmP,
    withoutFactor,
    lcmT,
    factor,
    simP,
    listElimination
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
-- newtype Poly t ord =
--   Poly
--     { getP :: [Term t ord]
--     }deriving (Eq)
newtype Poly t ord = Poly (Array N Ix1 (Term t ord)) deriving (Eq)
--p[Term 1 $ m[1,2,3], Term 3 $ m[4,5,6], Term 0 $ m[]] :: Poly Rational Revlex
--  Term 4 $ m[1,2,3,4] :: Term Rational Revlex

p :: [Term t ord] -> Poly t ord
p xs = Poly $ A.fromList Par xs


---------------------------------------------------- << FUNCTIONS >>-----------------------------------------
--- The class is independent of the monomial order, in theory but in Revlex we take the last elemens in a monomial
class' :: Poly t ord -> Int
class' (Poly p) = A.maximum' $ A.map (f) p
  where
    g (Mon m) = m
    f (Term k mon) = A.elemsCount (g mon)
--class' xs = max' $ P.map (elemsCount . getMon . snd . getT) (getP xs)
max' :: [Int] -> Int
max' [] = error "There is no max in a empty list"
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
lv = class'
--  LEX
--lv :: Poly t Lex -> String
--lv p =  "x_{" ++ show ( p)  ++ "}"
--------------------------------------------------------------
-- leading degreee
ld :: Poly t Revlex -> Int
ld p@(Poly xp) = max' .  P.map last' . toListP $ A.computeAs N predicate
  where
    toListP = A.toList . A.map h
    g (Mon m) = m
    f (Term k mon) =  A.elemsCount (g mon)
    h (Term k mon) = A.toList   (g mon)
    predicate = A.filterS (\x -> (f x) == class' p)  xp

-- ld :: Poly t Revlex -> Int
-- ld xp =
--   max' $
--   P.map
--     last'
--     [ A.toList x
--     | x <- P.map (getMon . snd . getT) (getP xp)
--     , class' xp == elemsCount x
--     ]
-- LEX
-- ld1 :: Poly t Lex -> Int
-- ld1 xp =
--   max' $
--   P.map
--     last'
--     [ A.toList x
--     | x <- P.map (getMon . snd . getT) (getP xp)
--     , class' xp == elemsCount x
--     ]
-- --------------------------------------------------------------
-- -- multidegree
-- --mdP :: Poly t ord -> [Int]
-- --mdP = undefined
-- --------------------------------------------------------------
-- leading term
-- lt :: (Eq t) => Poly t Revlex -> Term t Revlex
-- lt xp = lmMax' [ x | x <- getP xp, ld xp == f x ]
--   where
--     f = last' . A.toList . getMon . snd . getT

-- lmMax' :: (Eq t )=>[Term t Revlex] -> Term t Revlex
-- lmMax' [x] = x
-- lmMax' (x:x':xs) = lmMax'
--     ((if x >= x'
--         then x
--         else x') :
--      xs)
--MULTIVARIABLE -------------------------------------------------
-- -- ---------------------------------------------------------------
-- lm :: (Num t, Eq t ) => Poly t Revlex -> Term t Revlex
-- lm xp =  Term (1, xs)
--   where
--     (x,xs) = getT $ lt xp
-- -- --------------------------------------------------------------
-- -- -- Initial of a palynomial with respect to a variable
initOfv :: (Eq t) => Poly t Revlex -> Poly t Revlex
initOfv p@(Poly xp) = Poly $ A.computeAs N (A.map takeInit (A.computeAs N predicate))
  where
    predicate =  A.filterS (\x -> f x == class' p && h x == ld p) xp
    f (Term k mon) = A.elemsCount (g mon)
    g (Mon m) = m
    h (Term k mon) =  g mon ! (A.elemsCount (g mon) P.- 1 )

takeInit :: Term t ord -> Term t ord
takeInit (Term k mod)
  | A.elemsCount (g mod)  == 1 = Term k $ m[]
  | otherwise = Term k $ Mon ( A.computeAs P ( A.takeS (Sz (A.elemsCount (g mod)) P.- 1 ) (g mod) ))
  where
    g (Mon m) = m

initOfv' :: (Eq t) => Poly t Revlex -> Poly t Revlex
initOfv' p@(Poly xp) = Poly $  (A.computeAs N predicate)
  where
    predicate =  A.filterS (\x -> f x == class' p && h x == ld p) xp
    f (Term k mon) = A.elemsCount (g mon)
    g (Mon m) = m
    h (Term k mon) =  g mon ! (A.elemsCount (g mon) P.- 1 )

-- --------------------------------------------------------------

-- lc :: (Num t, Eq t ) => Poly t Revlex -> Term t Revlex
-- lc xp =  Term (x, m[0])
--   where
--     (x,xs) = getT $ lt xp
-- --------------------------------------------------------------
-- -- reduction of the leading monomial
-- red :: (Eq t) => Poly t Revlex -> Poly t Revlex
-- red xs = Poly [x | x <- getP xs, x /= a]
--   where
--     a = lt xs

-- ------------------------------------------------------------
-- --Monomial exponentation
-- -- expM :: (Num k) => Term k ord -> Int -> Term k ord
-- -- expM m i = Term (p, Mon $ A.map (P.*i) b)
-- --   where
-- --     (k,m') = getT m
-- --     p = foldl (P.*) 1 $ L.replicate i k
-- --     b  = getMon m'
-- --------------------------------------------------------------
-- --- comparison total degree (muti degree monomial)
-- mdM :: Term k ord -> [Int]
-- mdM = toList . getMon . snd .  getT
-- --------------------------------------------------------------
-- lcm para polynomios
lcmP :: (Num t, Ord t)=> Poly t  Revlex -> Poly t  Revlex -> [Poly t  Revlex]
lcmP a@(Poly poly) b@(Poly poly')
  | A.elemsCount poly == 1 && A.elemsCount poly' == 1 = (p $ on lcmT (head . A.toList) poly poly' : []) :[]
  | A.elemsCount poly' == 1 = withoutFactor a N.* (p $ lcmT (head . A.toList $ poly') (factor a) : []) :[]
  | A.elemsCount poly == 1 = withoutFactor b N.* (p $ lcmT (head . A.toList $ poly) (factor b) : []) :[]
  | otherwise = [p $ [on lcmT (head . A.toList) poly poly'],withoutFactor a, withoutFactor b]

factor :: (Num t) => Poly t Revlex -> Term t Revlex
factor (Poly poly) = Term 1 $ m commList
  where
    f (Term k mon) = g mon
    g (Mon m) = A.toList m
    commList = L.foldl1' (P.zipWith min)  (A.toList $ A.map f poly)


withoutFactor ::(Num t) => Poly t Revlex -> Poly t  Revlex
withoutFactor (Poly poly) = Poly $ A.computeAs N ( A.map f poly)
  where
    f m = quit m m'
    m' = factor $ Poly poly

quit :: Term k ord -> Term k ord -> Term k ord
quit (Term k mon) (Term k' mon')
  | mon' == zero = Term k mon
  | otherwise = Term k $ m (on quit' f mon mon')
  where
    f (Mon m) = A.toList m

quit' :: [Int]->[Int]->[Int]
quit' as [] =  as
quit' (a:as) (b:bs) = a P.- b : quit' as bs


-- lcm'' :: (Ord t, Num t )=> [Term t Revlex] -> [Term t Revlex] -> [Term  t Revlex]
-- lcm'' xs xp
--   | length xs == 1 && length xp == 1 = [ on lcmT head xs xp]
--   | otherwise = undefined

-- --------------------------------------------------------------
lcmT ::  (Ord t, Num t) => Term t Revlex -> Term t Revlex  -> Term t Revlex
lcmT (Term k mon)(Term k' mon')= lcm' mon mon'

lcm' :: (Ord t, Num t )=> Mon Revlex -> Mon Revlex -> Term t Revlex
lcm' (Mon m)(Mon m') = Term 1 $ Mon (A.computeAs P (A.zipWith max m m' ))
--------------------------------------------------------------
-- mon :: Term t ord -> Mon ord
-- mon m = Mon $ getMon b
--   where
--     (a,b) = getT m
-- --------------------------------------------------------------
-- -- spolynomial
basicSpoly ::(Fractional t, Num t, Eq t, Ord t ) =>  Poly t Revlex -> Poly t Revlex -> Poly t Revlex
basicSpoly f g = simP x'  (initOfv' f)  f N.- simP x' (initOfv' g)  g
  where
    x'  = on lcmP initOfv' f g

--length (getP f) P.> 1 && length (getP g) P.> 1 = [ withoutFactor f, withoutFactor g,  Poly[lcmT (factor f) (factor g)] ]

simP :: (Fractional t, Num t, Eq t ) => [Poly t Revlex]->Poly t Revlex->Poly t Revlex->Poly t Revlex
simP f1 f2 f3 
  | length f1 == 3 = listElimination f1 f2 f3
  | (orden . head $ f1) == orden f2 = f3 --si
  | orden f3 == orden f2 =   head f1   --si
  | (f . head $ f1) == 1 && (f f2) == 1 = p[h f1 N./ g f2] N.*  f3 --tiene sentido que se pregunte por f1 == 1 porque este puede ser un polynomio
  | f f2 == 1 = b N.* p [a N./  g f2] N.* f3
  | orden  b == orden b' = p[a N./ a'] N.* f3
  | otherwise = error "Option not consider in simP"
    where
       (a,b) = (factor . head $ f1, (withoutFactor . head) f1)
       (a',b') = (factor f2, withoutFactor f2)
       f (Poly poly) = A.elemsCount poly
       g (Poly poly) = head . A.toList $ poly
       h [f1] = g f1



orden :: Poly t Revlex -> Poly t Revlex
orden (Poly poly) = p $ P.map h ( sortBy (\( k, mon) ( k', mon') -> compare mon mon') (A.toList $ A.map f  poly ))
     where
       h (k, xs)=  Term k $ m xs
       g (Mon m) = m
       f (Term k mon) = (k,  A.toList (g mon))

listElimination :: (Fractional t, Eq t, Num t) => [Poly t Revlex] -> Poly t Revlex -> Poly t Revlex -> Poly t Revlex
listElimination [a,b,c] f2 f3
  | orden b == orden e = f3 N.* p[ (head . A.toList $ f a) N./ d] N.* c
  | orden c == orden e = f3 N.* p[ (head . A.toList $ f a) N./ d]  N.* b
  | orden f3 == orden f2 = a N.* b N.* c
  | otherwise = error "Option not consider list Elimination"
  where
    (d,e) = (factor f2, withoutFactor f2)
    f (Poly poly) = poly

spoly :: (Fractional t, Ord t) => Poly t Revlex -> Poly t Revlex -> Poly t Revlex
spoly f g
  | f == zero = f
  | ld f >= ld g && class' g == class' f  = spoly (basicSpoly f g) g
  | otherwise = f
------------------------------------------------------------
-- pseudo remainder
prem :: (Eq t, Num t) => Poly t Revlex -> Poly t Revlex -> Poly t Revlex
prem g f
  | g /= zero && class' g == class' f && ld g >= m = prem calc f
  | otherwise = g
  where
    m = ld f
    calc = (initOfv f N.* g) N.- (initOfv g N.* f N.* p [Term 1 $ mp[lv f][ld g P.- m] ])
------ <<INSTANCES >> ------------------------------------
instance (Ord t, Num t, Show t) => Show (Poly t ord) where
  show (Poly p) = showPoly p

showPoly :: (Ord t, Num t, Show t) => Array N Ix1 (Term t ord)  -> String
showPoly terms =  (concat .  A.toList) $ A.map f terms
  where
    f (Term k mon)
      | k P.> 0 = " + " ++ show k ++ show mon
      | k P.< 0 = " + " ++ show k ++ show mon
      | otherwise = error "term with empty coefficient k"

-- ---------------------------------------------------------------
instance (Num t, Eq t) => Additive (Poly t Revlex) where
  (+) (Poly poly)(Poly poly') =
    p $
    P.filter removeZero $
    P.map tsum $
    groupBy (\( k, mon) ( k', mon') -> (==) mon mon') $
    sortBy (\( k, mon) ( k', mon') -> compare mon mon') (A.toList $ A.map f  (computeAs N concatp ))
     where
       g (Mon m) = m
       f (Term k mon) = (k,  A.toList (g mon))
       concatp = A.append' 1 poly poly'
--sum term in a polynomial
tsum :: (Num k) => [(k, [Int])] -> Term k ord
tsum [(k,x)] = Term k (m x)
tsum ((k, x):xs) = (Term k $ m x) N.+ tsum xs

-- remove term 0 form a polynomial
removeZero :: (Num k, Eq k) => Term k ord -> Bool
removeZero (Term k mon)
  | k /= 0 = True
  | otherwise = False


instance (Num t, Eq t) => Semiring (Poly t Revlex)
instance (Num t, Eq t) => Abelian (Poly t Revlex)
instance (Num t, Eq t) => Multiplicative (Poly t Revlex) where
  (*) (Poly poly)(Poly poly') = simp $ p [ x N.* y | x <- A.toList poly, y <- A.toList poly' ]

simp :: (Num t, Eq t) => Poly t Revlex -> Poly t Revlex
simp (Poly poly) =
    p $
    P.filter removeZero $
    P.map tsum $
    groupBy (\( k, mon) ( k', mon') -> (==) mon mon') $
    sortBy (\( k, mon) ( k', mon') -> compare mon mon') (A.toList $ A.map f  poly )
     where
       g (Mon m) = m
       f (Term k mon) = (k,  A.toList (g mon))
---------------------------------------------------------------------------
-- -- instance Division (Mon ord) where
-- --   (/) xs xz =

-- -- λ> c = Poly [Term(3,m[1,2]), Term(5,m[2,3])] :: Poly Int Lex
-- -- λ> d = Poly [Term(2,m[2,1]), Term(2,m[3,5])] :: Poly Int Lex
-- -- λ> c N.* d
-- -- +6x₁³x₂³+6x₁⁴x₂⁷+10x₁⁴x₂⁴+10x₁⁵x₂⁸
-- -- λ>
-- -- 22 marzo bryan

instance (Num t, Eq t) => Group (Poly t Revlex) where
  (-) (Poly poly)(Poly poly') =
    p $
    P.filter removeZero $
    P.map tsum $
    groupBy (\( k, mon) ( k', mon') -> (==) mon mon') $
    sortBy (\( k, mon) ( k', mon') -> compare mon mon') (A.toList $ A.map f  (computeAs N concatp ))
     where
       g (Mon m) = m
       f (Term k mon) = (k,  A.toList (g mon))
       h (Term k mon) = Term (- k) mon
       concatp = A.append' 1 poly  (A.map h poly')


instance (Num t, Eq t) => LeftModule Integer (Poly t Revlex) where
  (.*) = undefined
instance (Num t, Eq t) => RightModule Integer (Poly t Revlex) where
  (*.) = undefined
instance (Num t, Eq t) => LeftModule Natural (Poly t Revlex) where
  (.*) = undefined
instance (Num t, Eq t) => RightModule Natural (Poly t Revlex) where
  (*.) = undefined

instance (Num t, Eq t) => Monoidal (Poly t Revlex) where
   zero = p[] --Poly (Array N Ix1 m[]) --zero --p[]


last' ::  [Int] -> Int
last' [] = 0
last' xs = last xs

-- --------------------------FACTOR-----------------
-- -- prime_factors :: Int -> [Int]

-- -- prime_factors 1 = []
-- -- prime_factors n
-- --   | factors == []  = [n]
-- --   | otherwise = factors ++ prime_factors (n `div` (head factors))
-- --   where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n P.- 1]

-- -- mcd' :: [Int] -> [[Int]]
-- -- mcd' ps = P.map (prime_factors) ps

-- -- mcd :: [Int] -> Int
-- -- mcd ps =  P.foldl1 (P.*) $ P.foldl1 mezc' (mcd' ps)

-- -- mezc' :: [Int] -> [Int] -> [Int]
-- -- mezc' [] _ = []
-- -- mezc' _ [] =  []
-- -- mezc' (x:xs)(y:ys)
-- --   | x P.< y       = mezc' xs (y:ys)
-- --   | x P.> y       = mezc'(x:xs) ys
-- --   | x == y      = [x] ++ mezc' xs ys
-- ------------------------------------------------

-- --------------------------FACTOR-----------------
-- -- a =  Poly [Term(1,m[1,2]), Term(2,m[3,4]), Term(4,m[1,2])] :: Poly Int Lex
-- -- sortBy (\ (Term (_,b)) (Term(_,d)) -> compare b d) $ getP a

-- -- poly = sum of terms
-- -- temr = coeff * monomio
-- -- monomio = set of univariantes
-- -- lm = highest value in the monomial
-- -- f = Poly [Term (1,m[1,2]), Term(1, m[0])] :: Poly Int Revlex
-- -- g = Poly [Term (2,mp[2][3]), Term(-1, mp[2][2]), Term(1,m[2,1])] :: Poly Int Revlex

-- --  Poly [Term(4,m[1,2]), Term(-3,m[5,6]), Term(6,mp[4][9])] :: Poly Int Lex
-- -- Poly [Term(4,m[1,2]), Term(-3,m[5,10]), Term(6,mp[4][9]), Term(15, mp[1,3][3,9]), Term(30, mp[3,4,5][1,2,3]), Term(34, mp[3,4,5][2,2,3])] :: Poly Int Lex
-- -- a = Poly [Term(4,m[1,2]), Term(-3,m[5,10]), Term(6,mp[4][9]), Term(15, mp[1,3][3,9]), Term(30, mp[2,3,4,5][4,1,2,3]), Term(34, mp[2,3,4,5][8,2,2,3])] :: Poly Int Lex
-- -- c = Poly [Term(2,m[2,1]), Term(1, m[1,2])] :: Poly Int Revlex
-- -- f = Poly [Term(1,m[2,3]), Term(-1,m[0,1])] :: Poly Int Lex
-- -- g = Poly [Term(1,m[3,1]), Term(-2,m[])] :: Poly Int Lex

-- -- a = Poly [Term(1,m[2,1]), Term(1,m[1,2])] :: Poly Int Revlex
-- -- b = Poly [Term(1,m[3]), Term(2,m[2,1]), Term(1,mp[3][1]), Term(3,m[1,2])] :: Poly Int Revlex
-- -- c = Poly [Term(2,m[2,1]), Term(1, m[1,2])] :: Poly Int Revlex
-- --Ideals
-- -- >>>
-- -- f = Poly [Term(1,m[2,3]), Term(-1, mp[2][1])] :: Poly Int Revlex
-- -- g = Poly [Term(1,m[3,1]), Term(-2,m[0])]  :: Poly Int Revlex
-- -- q = Poly [Term(1,m[8,2]), Term(2,m[5,1]), Term(4,m[2]), Term(-1, m[6])] :: Poly Int Revlex
-- -- r = Poly [Term(8,m[2]),Term(-2,m[6])] :: Poly Int Revlex
-- -- a = Poly [expM (Term(1,m[3]) :: Term Int Revlex) 3]
-- -- example spoly (int) Ans: (-15) % 2 + 10 % 1x₁¹(-5) % 2x₁²
-- -- f1 = Poly [Term(2,mp[2][2]),Term(-4,mp[2][1]), Term(1,m[2]), Term(-4,m[1]), Term(3,m[0])] :: Poly Rational Revlex
-- -- f2 = Poly [Term(1,mp[2][2]), Term(-2,mp[2][1]),Term(3,m[2]),Term(-12,m[1]), Term(9,m[0])] :: Poly Rational Revlex
-- -- example Cox 350 Ans:
-- --p1 = Poly [Term (1,m[2,3]), Term(-1,mp[2][1])] :: Poly Rational Revlex
-- --p2 = Poly [Term (1,m[3,1]), Term(-2,m[0])] :: Poly Rational Revlex
-- -- repect to y
f1 = p [Term 1 $ m[2,3] , Term (- 1) $ mp[2][1] ] :: Poly Rational Revlex
f2 = p [Term 1 $ m[3,1] , Term (- 2) $ m[0] ] :: Poly Rational Revlex
-- asn prem 8x^2 -2x^6
-- poly f1 f2 = 8 -2x⁴
-- -- poly to taste the initial term
-- -- f1 = Poly [Term(4,m[2,1]),Term(-1,m[6,1])] :: Poly Int Revlex
-- -- buchberger example 19
-- -- f1 = Poly [Term(1,m[1,2]), Term(1,m[0])] :: Poly Int Revlex
-- -- f2 = Poly [Term(2,mp[2][3]), Term(-1,mp[2][2]), Term(1,m[2,1])] :: Poly Int Revlex
-- -- dividendo primero divisor despues
-- -- BUCH
-- -- p1 = Poly [Term(1,mp[1,4][1,1]), Term(1,mp[3][1]), Term(1,m[1,1])] :: Poly Rational Revlex
-- -- p2 = Poly [Term(2,mp[4][2]), Term(-2,mp[3,4][1,1]), Term(5,mp[1,2,4][1,1,1]), Term(-5,m[1,1,1])] :: Poly Rational Revlex
-- -- p3 = Poly [Term(1,m[1,3]), Term(-2,m[2,2]), Term(1,mp[2][1])] :: Poly Rational Revlex
-- -- p4 = Poly [Term(3,mp[2][4]), Term(-1,m[1])] :: Poly Rational Revlex
-- ps = [Poly[Term(1,m[2])]] :: [Poly Rational Revlex]
-- ps1 = [Poly[Term(1,m[2])]] :: [Poly Rational Revlex]
