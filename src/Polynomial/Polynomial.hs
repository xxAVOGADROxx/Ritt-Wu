{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}


module Polynomial.Polynomial
  (
    Poly(..),
    class',
    ld,
    prem,
    varDegree,
    lt,
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
    listElimination,
    p,
    orden,
    tsum,
    totalDeg,
    numTerms
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
import Control.DeepSeq
import GHC.Generics (Generic, Generic1)


newtype Poly t ord = Poly (Array N Ix1 (Term t ord)) deriving (Generic, NFData, Eq)
--p[Term 1 $ m[1,2,3], Term 3 $ m[4,5,6], Term 0 $ m[]] :: Poly Rational Revlex
--  Term 4 $ m[1,2,3,4] :: Term Rational Revlex
--instance NFData (Poly t  ord) where
--  rnf x = seq x ()

p :: (NFData k ) => [Term k Revlex] -> Poly k Revlex
p xs = Poly $ A.fromList (ParN 2) xs


---------------------------------------------------- << FUNCTIONS >>-----------------------------------------
--- The class is independent of the monomial order, in theory but in Revlex we take the last elemens in a monomial
class' ::(NFData t ) => Poly t ord -> Int
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
lv :: (NFData t ) => Poly t Revlex -> Int
lv = class'
--  LEX
--lv :: Poly t Lex -> String
--lv p =  "x_{" ++ show ( p)  ++ "}"
--------------------------------------------------------------
-- leading degreee
ld :: (NFData t ) => Poly t Revlex -> Int
ld p@(Poly xp) =  A.maximum' . findingLast  $ A.computeAs N predicate
  --max' .  P.map last' . toListP $ A.computeAs N predicate
  where
    -- toListP = A.toList . A.map h
    g (Mon m) = m
    f (Term k mon) =  A.elemsCount (g mon)
    -- h (Term k mon) = A.toList   (g mon)
    predicate = A.sfilter (\x -> (f x) == class' p)  xp
    k (Term k mon) = if elemsCount (g mon) /= 0 then  g mon ! ((elemsCount (g mon)) P.-1) else 0
    findingLast = A.map k


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
--leading term
lt :: (NFData t, Eq t) => Poly t Revlex -> Term t Revlex
lt poly@(Poly terms)= h . i . computeAs N $  fil terms
  where
    fil = A.sfilter (\x -> f x == ld poly)
    g (Mon m) = m
    f (Term k mon) = if elemsCount (g mon) /= 0 then  g mon ! (elemsCount (g mon) P.-1) else 0
    i = A.quicksort
    h = head . A.toList

varDegree :: (NFData t, Eq t) => Poly t Revlex -> Int -> Int
varDegree (Poly terms ) n = maxel . A.map takeEl . computeAs N $ fil terms
  where
    fil = A.sfilter (\x -> f x >= n)
    g (Mon m) = m
    f (Term k mon) = if elemsCount (g mon) /= 0 then  elemsCount (g mon) else 0
    maxel = maximum'
    takeEl (Term k mon) = g mon ! (n P.- 1)

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
initOfv :: (NFData t, Eq t) => Poly t Revlex -> Poly t Revlex
initOfv p@(Poly xp) = Poly $ A.computeAs N (A.map takeInit (A.computeAs N predicate))
  where
    predicate =  A.sfilter (\x -> f x == class' p && h x == ld p) xp
    f (Term k mon) = A.elemsCount (g mon)
    g (Mon m) = m
    h (Term k mon) =  g mon ! (A.elemsCount (g mon) P.- 1 )

takeInit :: Term t ord -> Term t ord
takeInit (Term k mod)
  | A.elemsCount (g mod)  == 1 = Term k $ m[]
  | otherwise = Term k $ Mon ( A.computeAs P ( A.take (Sz (A.elemsCount (g mod)) P.- 1 ) (g mod) ))
  where
    g (Mon m) = m

initOfv' :: (NFData t, Eq t) => Poly t Revlex -> Poly t Revlex
initOfv' p@(Poly xp) = Poly $  (A.computeAs N predicate)
  where
    predicate =  A.sfilter (\x -> f x == class' p && h x == ld p) xp
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
lcmP :: (NFData t, Num t, Ord t)=> Poly t  Revlex -> Poly t  Revlex -> [Poly t  Revlex]
lcmP a@(Poly poly) b@(Poly poly')
  | A.elemsCount poly == 1 && A.elemsCount poly' == 1 = (p $ on lcmT (head . A.toList) poly poly' : []) :[]
  | A.elemsCount poly' == 1 = withoutFactor a N.* (p $ lcmT (head . A.toList $ poly') (factor a) : []) :[]
  | A.elemsCount poly == 1 = withoutFactor b N.* (p $ lcmT (head . A.toList $ poly) (factor b) : []) :[]
  | otherwise = [p $ [on lcmT factor a b],withoutFactor a, withoutFactor b]

factor :: (NFData t, Num t) => Poly t Revlex -> Term t Revlex
factor (Poly poly) = Term 1 $ m commList
  where
    f (Term k mon) = g mon
    g (Mon m) = A.toList m
    commList = L.foldl1' (P.zipWith min)  (A.toList $ A.map f poly)


withoutFactor ::(NFData t, Num t) => Poly t Revlex -> Poly t  Revlex
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
basicSpoly ::(NFData t, Fractional t, Num t, Eq t, Ord t ) =>  Poly t Revlex -> Poly t Revlex -> Poly t Revlex
basicSpoly f g = simP x'  (initOfv' f)  f N.- simP x' (initOfv' g)  g
  where
    x'  = on lcmP initOfv' f g

--length (getP f) P.> 1 && length (getP g) P.> 1 = [ withoutFactor f, withoutFactor g,  Poly[lcmT (factor f) (factor g)] ]

simP :: (NFData t, Fractional t, Num t, Eq t ) => [Poly t Revlex]->Poly t Revlex->Poly t Revlex->Poly t Revlex
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



orden :: (NFData t)=> Poly t Revlex -> Poly t Revlex
orden (Poly poly) = p $ P.map h ( sortBy (\( k, mon) ( k', mon') -> compare mon mon') (A.toList $ A.map f  poly ))
     where
       h (k, xs)=  Term k $ m xs
       g (Mon m) = m
       f (Term k mon) = (k,  A.toList (g mon))

listElimination :: (NFData t, Fractional t, Eq t, Num t) => [Poly t Revlex] -> Poly t Revlex -> Poly t Revlex -> Poly t Revlex
listElimination [a,b,c] f2 f3
  | orden b == orden e = f3 N.* p[ (head . A.toList $ f a) N./ d] N.* c
  | orden c == orden e = f3 N.* p[ (head . A.toList $ f a) N./ d]  N.* b
  | orden f3 == orden f2 = a N.* b N.* c
  | otherwise = error "Option not consider list Elimination"
  where
    (d,e) = (factor f2, withoutFactor f2)
    f (Poly poly) = poly

spoly :: (NFData t, Fractional t, Ord t) => Poly t Revlex -> Poly t Revlex -> Poly t Revlex
spoly f g
  | f == zero = f
  | ld f >= ld g && class' g == class' f  = spoly (basicSpoly f g) g
  | otherwise = f
------------------------------------------------------------
-- pseudo remainder
prem :: (NFData t, Eq t, Num t) => Poly t Revlex -> Poly t Revlex -> Poly t Revlex
prem g f
  | g /= zero && class' g == class' f && ld g >= m = prem calc f
  | otherwise = g
  where
    m = ld f
    calc = (initOfv f N.* g) N.- (initOfv g N.* f N.* p [Term 1 $ mp[lv f][ld g P.- m] ])
------ <<INSTANCES >> ------------------------------------
instance (NFData t, Ord t, Num t, Show t) => Show (Poly t ord) where
  show (Poly p) = showPoly p

showPoly :: (NFData t, Ord t, Num t, Show t) => Array N Ix1 (Term t ord)  -> String
showPoly terms =  (concat .  A.toList) $ A.map f terms
  where
    f (Term k mon)
      | k P.> 0 = " + " ++ show k ++ show mon
      | k P.< 0 = " - " ++ show (abs k) ++ show mon
      | otherwise = error "term with empty coefficient k"

-- ---------------------------------------------------------------
instance (NFData t, Num t, Eq t) => Additive (Poly t Revlex) where
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


instance (NFData t, Num t, Eq t) => Semiring (Poly t Revlex)
instance (NFData t, Num t, Eq t) => Abelian (Poly t Revlex)
instance (NFData t, Num t, Eq t) => Multiplicative (Poly t Revlex) where
  (*) (Poly poly)(Poly poly') = simp $ p [ x N.* y | x <- A.toList poly, y <- A.toList poly' ]

simp :: (NFData t, Num t, Eq t) => Poly t Revlex -> Poly t Revlex
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

instance (NFData t, Num t, Eq t) => Group (Poly t Revlex) where
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


instance (NFData t, Num t, Eq t) => LeftModule Integer (Poly t Revlex) where
  (.*) = undefined
instance (NFData t, Num t, Eq t) => RightModule Integer (Poly t Revlex) where
  (*.) = undefined
instance (NFData t, Num t, Eq t) => LeftModule Natural (Poly t Revlex) where
  (.*) = undefined
instance (NFData t, Num t, Eq t) => RightModule Natural (Poly t Revlex) where
  (*.) = undefined

instance (NFData t, Num t, Eq t) => Monoidal (Poly t Revlex) where
   zero = p[] --Poly (Array N Ix1 m[]) --zero --p[]


last' ::  [Int] -> Int
last' [] = 0
last' xs = last xs


-- find the term with the maximum of the sum of all powers
totalDeg :: (NFData t) => Poly t Revlex -> Int
totalDeg (Poly p) = A.maximum' $ A.map (f) p
  where
    g (Mon m) = m
    f (Term k mon) = A.sum (g mon)
-- Contador de terminos en un polinomio
numTerms :: (NFData t) => Poly t Revlex -> Int
numTerms (Poly p) = A.elemsCount p
