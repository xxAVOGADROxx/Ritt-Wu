module Polynomial.Wu () where
import Control.Scheduler
import Polynomial.Monomial
import Polynomial.Terms
import Polynomial.Polynomial
import Data.List
import Data.Function
--import Numeric.Algebra as N
-- import Prelude as P
-- Group the polynomial set PS in different classes
stage3 :: [Poly t Revlex] -> [[Poly t Revlex]]
stage3  xs =  quitTuple . sortGrupPolyClass . searchPolyClass $ xs

searchPolyClass :: [Poly t Revlex] -> [ (Poly t Revlex, Int )]
searchPolyClass xs = [(x, class' x) |x <- xs]

sortGrupPolyClass ::  [ (Poly t Revlex, Int )] ->  [[ (Poly t Revlex, Int )]]
sortGrupPolyClass xs = groupBy (\ a b -> on (==) snd a b) $  sortBy (\ a b -> on compare snd a b ) xs

quitTuple ::  [[ (Poly t Revlex, Int )]] -> [[Poly t Revlex]]
quitTuple xs =   [ map fst x | x <- xs]
-----------------------------------------------------------------------------
-- take a Basic Set BS from a ascSet
basicSet = undefined
-- search the polynomial with lower degree in a list of polynomials with the same class
lowerDegreePoly :: [Poly t Revlex] -> Poly t Revlex
lowerDegreePoly xs =  g . f $ xs

f :: [Poly t Revlex] -> [ (Poly t Revlex, Int )]
f xs = [ (x, ld x)| x <- xs]

g :: [(Poly t Revlex, Int)] -> Poly t Revlex 
g xs = fst . head $ sortBy (\a b -> on compare snd a b ) xs


-----------------------------------------------------------------------------
chaSet :: [Poly t Revlex] -> [Poly t Revlex]
chaSet = undefined
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
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
-- f1 = Poly [Term(1,mp[1,4][1,2]), Term(1,mp[4][2]), Term(-1,mp[1,2,4][1,1,1]), Term(-1,mp[2,4][1,1]), Term(1,m[1,1]), Term(3, mp[2][1])] :: Poly Rational Revlex
-- f2 = Poly [Term(1,mp[1,4][1,1]), Term(1,mp[3][1]), Term(-1,m[1,1])] :: Poly Rational Revlex
-- f3 = Poly [Term(1,mp[3,4][1,1]), Term(-2,mp[2][2]),Term(-1,m[1,1]),Term(-1,m[0])] :: Poly Rational Revlex
-- f4 = Poly [] :: Poly Rational Revlex
