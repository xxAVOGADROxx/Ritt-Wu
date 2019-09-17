module Polynomial.Wu () where
import Control.Scheduler
import Polynomial.Monomial
import Polynomial.Terms
import Polynomial.Polynomial
import Data.List as L
import Data.Function
import Data.Massiv.Array as A
--import Numeric.Algebra as N
-- import Prelude as P
-- Group the polynomial set PS in different classes
setClassPs :: [Poly t Revlex] -> [[Poly t Revlex]]
setClassPs  xs =  quitTuple . sortGrupPolyClass . searchPolyClass $ xs

searchPolyClass :: [Poly t Revlex] -> [ (Poly t Revlex, Int )]
searchPolyClass xs = [(x, class' x) |x <- xs]

sortGrupPolyClass ::  [ (Poly t Revlex, Int )] ->  [[ (Poly t Revlex, Int )]]
sortGrupPolyClass xs = groupBy (\ a b -> on (==) snd a b) $  sortBy (\ a b -> on compare snd a b ) xs

quitTuple ::  [[ (Poly t Revlex, Int )]] -> [[Poly t Revlex]]
quitTuple xs =   [ L.map fst x | x <- xs]
-----------------------------------------------------------------------------
-- take a Basic Set BS from a PS (but only lower rank )
-- basicSet' :: (Num t, Eq t) => [[Poly t Revlex]] -> [Poly t Revlex]
-- basicSet' xs = searchLowerRank xs []
------------------------------------------------
-- -- a function that search a poly with lower rank and reduced x_i
-- search Poly Lower Rank and Reduced
sPLRR ::(Num t, Eq t)=> [[Poly t Revlex]] -> [Poly t Revlex] -> [Poly t Revlex]
sPLRR (x:xs) [] = sPLRR xs [head $ lDPolys x] 
sPLRR [] xp = xp
sPLRR xs xp 
  | h  /= Poly[] = sPLRR (tail xs) (h : xp)
  | otherwise = sPLRR (tail xs) xp
  where
    f = lDPolys . head
    h = reducedP (f xs) (head xp)
----------------------------------------------------------------
-- check if a given polynomial is reduced with respect to a set
reducedP :: (Num t, Eq t)=> [Poly t Revlex] -> Poly t Revlex ->  Poly t Revlex
reducedP [] _ = Poly []
reducedP (x:xs) z
  | isReduced x z == True  = x
  | otherwise = reducedP xs z
-----------------------------------------------------------------
-- izq the entering polynomial, right the forming basic set
-- give two polyomials a b check if a is reduced to b 
isReduced :: (Eq t, Num t) => Poly t Revlex -> Poly t Revlex -> Bool
isReduced x y 
  | degP x (class' y) < degP y (class' y) = True
  | otherwise = False
-----------------------------------------------------------------
-- compare the degree of two polynomials in a given variable
degP :: Poly t Revlex -> Int -> Int
degP xs s = max' [ f x | x <- (getP xs), class' (Poly [x]) >= s]
  where
    f x = (toList . getMon . snd . getT) x !! (s-1)
-- search the polynomial (S) with lower degree in a list of polynomials with the same class
lDPolys :: [Poly t Revlex] -> [Poly t Revlex]
lDPolys xs = h .  g . f $ xs

f :: [Poly t Revlex] -> [ (Poly t Revlex, Int )]
f xs = [ (x, ld x)| x <- xs]

g :: [(Poly t Revlex, Int)] -> [(Poly t Revlex, Int) ]
g xs =  head $  groupBy (\(a, b) (c, d) -> (==) b d) $ sortBy (\a b -> on compare snd a b ) xs

h :: [(Poly t Revlex, Int)]-> [Poly t Revlex]
h xs = [ fst x |x <- xs]
-----------------------------------------------------------------------------
----------------------------------------------------------------------------
-- take a basic set using the classification class 
basicSet :: (Num t, Eq t) => [Poly t Revlex] -> [Poly t Revlex]
basicSet xs =  sPLRR (setClassPs xs) []
-----------------------------------------------------------------------------
charSet :: (Num t, Eq t)=>[Poly t Revlex] -> [Poly t Revlex]
charSet ps
  | basicSet ps /= ps  = charSet a
  | otherwise = ps
  where
    a = bsDividePs ps  (basicSet ps)
-----------------------------------------------------------------------------
-- division of every pk by Bs in PS
bsDividePs :: (Num t, Eq t)=>[Poly t Revlex] -> [Poly t Revlex] -> [Poly t Revlex]
bsDividePs ps bs = d (red bs ps) bs ++ bs 
-----------------------------------------------------------------------------
-- auxiliar function that perform the division of each polynomial by the triangular form
d :: (Num t, Eq t)=>[Poly t Revlex] -> [Poly t Revlex] -> [Poly t Revlex]
d xs xp = [ sprem x xp | x <- xs]
-- sucesive pseudo division of a polynomial and a polynomial set
sprem :: (Num t, Eq t) =>Poly t Revlex -> [Poly t Revlex] -> Poly t Revlex
sprem rm [] = rm
sprem rm (b:bs)
  | a /= Poly [] = sprem a bs
  | otherwise = Poly[]
  where
    a = prem rm b
-----------------------------------------------------------------------------
-- reducction of two list of  polynomials
red :: (Eq t) => [Poly t Revlex] -> [Poly t Revlex] -> [Poly t Revlex]
red [] xp = xp
red (x:xs) xp = red xs (delete x xp)
-----------------------------------------------------------------------------

scheduleSums :: IO [Int]
scheduleSums =
  withScheduler (ParOn [1..4]) $ \ scheduler -> do
    scheduleWork scheduler $ pure (10 + 1)
    scheduleWork scheduler $ pure (20 + 2)
    scheduleWork scheduler $ pure (30 + 3)
    scheduleWork scheduler $ pure (40 + 4)
    scheduleWork scheduler $ pure (50 + 5)


-- graphics calculator examples
-- p1 = Poly [Term(1,mp[2][2]), Term(-1,m[1,1]), Term(-1,m[0])] :: Poly Rational Revlex
-- p2 = Poly [Term(1,m[2]), Term(-2,mp[3][1])]:: Poly Rational Revlex
-- p3 = Poly [Term(1,mp[3][2]), Term(-1,m[1,1]), Term(1,m[0])] :: Poly Rational Revlex
-- p4 = Poly [Term3,mp[2][5]] :: Poly Rational Revlex
-- ps = [Poly [Term(1,mp[2][2]), Term(-1,m[1,1]), Term(-1,m[0])] :: Poly Rational Revlex, Poly [Term(1,m[2]), Term(-2,mp[3][1])]:: Poly Rational Revlex, Poly [Term(1,mp[3][2]), Term(-1,m[1,1]), Term(1,m[0])] :: Poly Rational Revlex]
--Poly [Term(3,mp[2][5])] :: Poly Rational Revlex]
-- ps1 = [Poly [Term(1,mp[2][2]), Term(-1,m[1,1]), Term(-1,m[0])] :: Poly Rational Revlex, Poly [Term(1,m[2]), Term(-2,mp[3][1])]:: Poly Rational Revlex, Poly [Term(1,mp[3][2]), Term(-1,m[1,1]), Term(1,m[0])] :: Poly Rational Revlex, Poly [Term(3,mp[2][5])] :: Poly Rational Revlex]

-- Buchberger
-- f1 = Poly [Term(1,mp[1,4][1,2]), Term(1,mp[4][2]), Term(-1,mp[1,2,4][1,1,1]), Term(-1,mp[2,4][1,1]), Term(1,m[1,1]), Term(3, mp[2][1])] :: Poly Rational Revlex; f2 = Poly [Term(1,mp[1,4][1,1]), Term(1,mp[3][1]), Term(-1,m[1,1])] :: Poly Rational Revlex; f3 = Poly [Term(1,mp[3,4][1,1]), Term(-2,mp[2][2]),Term(-1,m[1,1]),Term(-1,m[0])] :: Poly Rational Revlex; f4 = prem f1 f2
-- asc
-- f1 = Poly [Term(1,m[5])] :: Poly Rational Revlex; f2 = Poly [Term(1,m[6]), Term(1,mp[2][1])] :: Poly Rational Revlex; f4 = Poly [Term(1,m[1]),Term(1,mp[2][3])]; f3 = Poly[Term(1,m[2])]  :: Poly Rational Revlex
-- lower = [Poly[Term(1,m[6])], Poly[Term(4,m[3])], Poly[Term(6,m[5])], Poly[Term(10,m[3])], Poly[Term(1, mp[2][3]), Term(2, m[3])], Poly[Term(5,mp[2][5])], Poly[Term(3, mp[3][2]), Term(4, mp[1][2])], Poly[Term(3, mp[3][1])]] 

-- c1 = [Poly[Term(1,m[6])], Poly[Term(4,m[3])], Poly[Term(6,m[5])], Poly[Term(10,m[3])] :: Poly Rational Revlex; c2 = Poly[Term(1, mp[1,2][2,1]), Term(2, m[3])], Poly[Term(5,mp[2][5])] :: Poly Rational Revlex; c3 =  Poly[Term(3, mp[3][2]), Term(4, mp[1][2])], Poly[Term(3, mp[3][1])]] :: Poly Rational Revlex

-- c1 = [Poly[Term(1,m[6])], Poly[Term(4,m[3])], Poly[Term(6,m[5])], Poly[Term(10,m[3])]] :: [Poly Rational Revlex]; c2 = [Poly[Term(1, mp[1,2][2,1]), Term(2, m[3])], Poly[Term(5,mp[2][5])], Poly[Term(3,mp[1,2][1,1])]] :: [Poly Rational Revlex]; c3 = [Poly[Term(3, mp[3][2]), Term(4, mp[1][2])], Poly[Term(3, mp[3][1])]] :: [Poly Rational Revlex]
-- ps = concat $ c1:c2:c3:[]
--
-- c1 = [Poly[Term(1,m[6])], Poly[Term(4,m[5])], Poly[Term(6,m[7])], Poly[Term(10,m[3])]] :: [Poly Rational Revlex]; c2 = [Poly[Term(4, mp[1,2][2,1])],Poly[Term(1, mp[1,2][2,1]), Term(2, m[3])], Poly[Term(5,mp[2][5])], Poly[Term(3,mp[1,2][3,4])]] :: [Poly Rational Revlex]; c3 = [Poly[Term(3, mp[3][11]), Term(4, mp[1][2])], Poly[Term(3, mp[3][10])], Poly [Term(5,mp[1,2,3][1,0,7])]] :: [Poly Rational Revlex]
-- calculator example
-- f1 = Poly[Term(1,mp[2][2]), Term(-1, m[1,1]), Term(-1, m[0])] :: Poly Rational Revlex; f2 = Poly[Term(1,m[2]), Term(-2,mp[3][1])] :: Poly Rational Revlex; f3 = Poly[Term(1,mp[3][2]), Term(-1,m[1,1]), Term(1,m[0])] :: Poly Rational Revlex
