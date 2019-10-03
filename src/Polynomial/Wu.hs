{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Polynomial.Wu (
psBsP,
psBsS,

psBsSParP,
psBsSParS,

psBsSParn1P,

psBsSSeqS,
psBsSSeqP,

psBsUParP,
psBsUParPUpio,
psBsUParS,

charSet,
charSetPfPr,
charSetPfS,

pallR,
pallL,

basicSet
                     ) where
import Control.Scheduler
import Polynomial.Monomial
import Polynomial.Terms
import Polynomial.Polynomial
import Data.List as L
import Data.Function
import Data.Massiv.Array as A
import Control.Monad
import Numeric.Algebra as N
import Prelude as P
import Control.Concurrent
import Control.DeepSeq
import Foreign.Marshal.Unsafe
import System.IO.Unsafe
--import Numeric.Algebra as N
-- import Prelude as P
-- Group the polynomial set PS in different classes
setClassPs :: [Poly t Revlex] -> [[Poly t Revlex]]
setClassPs =  quitTuple . sortGrupPolyClass . searchPolyClass

searchPolyClass :: [Poly t Revlex] -> [ (Poly t Revlex, Int )]
searchPolyClass xs = [(x, class' x) |x <- xs]

sortGrupPolyClass ::  [ (Poly t Revlex, Int )] ->  [[ (Poly t Revlex, Int )]]
sortGrupPolyClass xs = groupBy (on (==) snd) $  sortBy (on compare snd) xs

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
  | isReduced x z  = x
  | otherwise = reducedP xs z
-----------------------------------------------------------------
-- izq the entering polynomial, right the forming basic set
-- give two polyomials a b check if a is reduced to b 
isReduced :: (Eq t, Num t) => Poly t Revlex -> Poly t Revlex -> Bool
isReduced x y 
  | degP x (class' y) P.< degP y (class' y) = True
  | otherwise = False
-----------------------------------------------------------------
-- compare the degree of two polynomials in a given variable
degP :: Poly t Revlex -> Int -> Int
degP xs s = max' [ f x | x <- getP xs, class' (Poly [x]) >= s]
  where
    f x = (toList . getMon . snd . getT) x !! (s P.-1)
-- search the polynomial (S) with lower degree in a list of polynomials with the same class
lDPolys :: [Poly t Revlex] -> [Poly t Revlex]
lDPolys = h .  g . f 

f :: [Poly t Revlex] -> [ (Poly t Revlex, Int )]
f xs = [ (x, ld x)| x <- xs]

g :: [(Poly t Revlex, Int)] -> [(Poly t Revlex, Int) ]
g xs =  head $  groupBy (\(a, b) (c, d) -> (==) b d) $ sortBy (on compare snd) xs

h :: [(Poly t Revlex, Int)]-> [Poly t Revlex]
h xs = [ fst x |x <- xs]
-----------------------------------------------------------------------------
----------------------------------------------------------------------------
-- take a basic set using the classification class 
basicSet :: [Poly Rational Revlex] -> [Poly Rational Revlex]
basicSet xs =  sPLRR (setClassPs xs) []
-----------------------------------------------------------------------------
charSet :: [Poly Rational Revlex] -> [Poly Rational Revlex]
charSet ps
  | basicSet ps /= ps  = charSet a
  | otherwise = ps
  where
    a = bsDividePs ps  (basicSet ps)
-----------------------------------------------------------------------------
-- division of every pk by Bs in PS
bsDividePs ::(Num t, Eq t) =>[Poly t Revlex] -> [Poly t Revlex] -> [Poly t Revlex]
bsDividePs ps bs = psBsP (red bs ps) bs ++ bs 
-----------------------------------------------------------------------------
-- auxiliar function that perform the division of each polynomial by the triangular form
-- polynomial set by triangular form
psBsP :: (Num t, Eq t)=>[Poly t Revlex] -> [Poly t Revlex] -> [Poly t Revlex]
psBsP ps bs = [ sprem x bs | x <- ps]

psBsS :: (Fractional t, Ord t, Num t, Eq t)=>[Poly t Revlex] -> [Poly t Revlex] -> [Poly t Revlex]
psBsS ps bs = [ sspoly x bs | x <- ps]

----- *****************************-----------------------
-- function that paralellism the work

---------------------------PSSSSSSSSSSSSSSSSSSS------------
-- characteristic set in parallel computation using pseudo remainder
charSetPfPr :: [Poly Rational Revlex] -> [Poly Rational Revlex]
charSetPfPr ps
  | basicSet ps /= ps  = charSetPfPr a
  | otherwise = ps
  where
    a = bsDividePsPF ps  (basicSet ps)

bsDividePsPF ::( Num t, Eq t) =>[Poly t Revlex] -> [Poly t Revlex] -> [Poly t Revlex]
bsDividePsPF ps bs = psBsUParP (red bs ps) bs ++ bs  --psByTfLR
  
--poly set by  triangular form lehins recomendation
--psByTfLR ::  [Poly Rational Revlex] -> [Poly Rational Revlex] -> [Poly Rational Revlex]
--psByTfLR ps bs =  unsafeLocalState ( traverseConcurrently Par (\p -> p `deepseq` pure p) [sprem  x bs | x <- ps] :: IO [Poly Rational Revlex])
-- sin embargo no puedo trabajar con IO poly -> IO poly -> IO poly porque IO no es traversable indispensable para traverseConcurrently
psBsUParP :: (Num t, Eq t) =>[Poly t Revlex] -> [Poly t Revlex] -> [Poly t Revlex]
psBsUParP ps bs =  unsafeLocalState ( traverseConcurrently Par (\p -> p `deepseq` pure p) [sprem  x bs | x <- ps] )
-- ahora me pide el nfdata pra poly (sin siquiera haberlo puesto en el bench)

psBsUParPUpio :: (Num t, Eq t) =>[Poly t Revlex] -> [Poly t Revlex] -> [Poly t Revlex]
psBsUParPUpio ps bs =  unsafePerformIO ( traverseConcurrently Par (\p -> p `deepseq` pure p) [sprem  x bs | x <- ps] )

-------------------------------------PSSSSSSSSSSSSSSSSSSS-------------------------
-------------------------------------SPPPPPPPPPPPPPPPPPP--------------------------
--characteristic set Parallel Form Spoly
charSetPfS :: [Poly Rational Revlex] -> [Poly Rational Revlex]
charSetPfS ps
  | basicSet ps /= ps  = charSetPfS a
  | otherwise = ps
  where
    a = bsDividePsPfS ps  (basicSet ps)

bsDividePsPfS ::(Fractional t, Ord t, Num t, Eq t) =>[Poly t Revlex] -> [Poly t Revlex] -> [Poly t Revlex]
bsDividePsPfS ps bs = psBsUParS (red bs ps) bs ++ bs  --psByTfLR

psBsUParS :: (Fractional t, Num t, Eq t, Ord t) =>[Poly t Revlex] -> [Poly t Revlex] -> [Poly t Revlex]
psBsUParS ps bs = unsafeLocalState (traverseConcurrently Par (\p -> p `deepseq` pure p) [sspoly  x bs | x <- ps] )

-- psByTfLRtS :: (Fractional t, Num t, Eq t, Ord t) =>[Poly t Revlex] -> [Poly t Revlex] -> IO[Poly t Revlex]
-- psByTfLRtS ps bs =
--     (traverseConcurrently
--        Par
--        (`scheduleWorkId` (\i -> threadDelay 100000 >> pure i))
--        [sspoly x bs | x <- ps])

-- scheduleId = (`scheduleWorkId` (\ i -> threadDelay 100000 >>  pure i))

-- sucesive pseudo division of a polynomial and a polynomial set (SPOLY)
sspoly :: (Fractional t, Num t, Eq t, Ord t) => Poly t Revlex -> [Poly t Revlex] -> Poly t Revlex
sspoly rm [] = rm
sspoly rm (b:bs)
  | a /= Poly [] = sspoly a bs
  | otherwise = Poly[]
  where
    a = spoly rm b

-------------------------------------SPPPPPPPPPPPPPPPPPP-------------------------------
--wrapper ::  [Poly Rational Revlex] -> [Poly Rational Revlex] -> [Poly Rational Revlex]
--wrapper ps bs = unsafeDupablePerformIO $ psByTfLRIO ps bs 
-- Porque la funcion IO nencesita el NFData y no el wrapper ?

psBsSParP ::  [Poly Rational Revlex] -> [Poly Rational Revlex] -> IO[Poly Rational Revlex]
psBsSParP ps bs = traverseConcurrently Par (\p -> p `deepseq` pure p) [sprem  x bs | x <- ps] :: IO [Poly Rational Revlex]

psBsSParS ::  [Poly Rational Revlex] -> [Poly Rational Revlex] -> IO[Poly Rational Revlex]
psBsSParS ps bs = traverseConcurrently Par (\p -> p `deepseq` pure p) [sspoly  x bs | x <- ps] :: IO [Poly Rational Revlex]


psBsSSeqP ::  [Poly Rational Revlex] -> [Poly Rational Revlex] -> IO[Poly Rational Revlex]
psBsSSeqP ps bs = traverseConcurrently Seq (\p -> p `deepseq` pure p) [sprem  x bs | x <- ps] :: IO [Poly Rational Revlex]

psBsSSeqS ::  [Poly Rational Revlex] -> [Poly Rational Revlex] -> IO[Poly Rational Revlex]
psBsSSeqS ps bs = traverseConcurrently Seq (\p -> p `deepseq` pure p) [sspoly  x bs | x <- ps] :: IO [Poly Rational Revlex]

psBsSParn1P ::  [Poly Rational Revlex] -> [Poly Rational Revlex] -> IO[Poly Rational Revlex]
psBsSParn1P ps bs = traverseConcurrently (ParN 1) (\p -> p `deepseq` pure p) [sprem  x bs | x <- ps] :: IO [Poly Rational Revlex]

--psByTfLRIOt ::  [Poly t Revlex] -> [Poly t Revlex] -> IO[Poly t Revlex]
--psByTfLRIOt ps bs = traverseConcurrently Par (\p -> p `deepseq` pure p) [sprem  x bs | x <- ps] :: IO [Poly t Revlex]


instance NFData (IO[Poly Rational Revlex]) where
  rnf x = seq x ()
instance NFData (Poly t Revlex) where
  rnf x = seq x ()
-- psByTfPM :: (Num t, Eq t) =>  [Poly t Revlex] -> [Poly t Revlex]-> IO [Poly t Revlex]
-- psByTfPM ps bs = withScheduler Par $ \x -> do
--   p <- ps 
--   scheduleWork x $ pure (sprem p bs )

--psByTfP :: (Num t, Eq t) => [Poly t Revlex] -> [Poly t Revlex]-> IO [[Poly t Revlex]]
--psByTfP ps bs = withScheduler Par $ \x -> scheduleWork x $ pure ([(sprem x) bs | x <- ps]) -- paralelisa?

--psByTfPR :: (Num t, Eq t) => [Poly t Revlex] -> [Poly t Revlex]-> IO [[Poly t Revlex]]
--psByTfPR ps bs = withScheduler Par $ \x -> replicateM (length ps) $ scheduleWork x $ pure ([(sprem x) bs | x <- ps]) -- replica one single work 

--psByTfPS :: (Num t, Eq t) => [Poly t Revlex] -> [Poly t Revlex]-> IO [Poly t Revlex]
--psByTfPS ps bs = withScheduler Par $ \x -> do scheduleWork x $ pure ( sprem (ps !! 0) bs )
--                                              scheduleWork x $ pure ( sprem (ps !! 1) bs ) -- there is no way we can know the length of a set
-- como crear funciones de acuerdo al input

-- laia :: Int -> Int -> IO [Int]
-- laia a b= withScheduler Par $ \x -> replicateM_ a $ scheduleWork  x  $ pure (L.foldl' (P.+) 0 [0 .. b]) -- scheduleId x

-- laia' :: Int -> Int -> IO[Int]
-- laia' a b = withScheduler Par $ \x -> do replicateM a $ scheduleWork  x  $ pure (L.foldl' (P.+) 0 [0 .. b]) -- scheduleId x
-- -- scheduleId = (`scheduleWorkId` (\ i -> threadDelay 1000000 >> print i >> pure (L.foldl' (P.+) 0 [0 .. 100000] )))

-- laia'' :: Int -> IO [Int]
-- laia'' a =
--   withScheduler Par $ \x -> do
--     scheduleWork x $ pure (L.foldl' (P.+) 0 [0 .. div a 2])
--     scheduleWork x $ pure (L.foldl' (P.+) 0 [div a 2  .. a])
---------------- ************** -----------------------------
-- sucesive pseudo division of a polynomial and a polynomial set
sprem :: (Num t, Eq t) => Poly t Revlex -> [Poly t Revlex] -> Poly t Revlex
sprem rm [] = rm
sprem rm (b:bs)
  | a /= Poly [] = sprem a bs
  | otherwise = Poly[]
  where
    a = prem rm b
-----------------------------------------------------------------------------
-- reducction of two list of  polynomials
red :: (Eq t) => [Poly t Revlex] -> [Poly t Revlex] -> [Poly t Revlex]
red xs xp = foldl (flip delete) xp xs
--red [] xp = xp
--red (x:xs) xp = red xs (delete x xp)
-----------------------------------------------------------------------------
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
-- c1 = [Poly[Term(1,m[6])], Poly[Term(4,m[5])], Poly[Term(6,m[7])], Poly[Term(10,m[3])]] :: [Poly Rational Revlex]; c2 = [Poly[Term(4, mp[1,2][2,1])],Poly[Term(1, mp[1,2][2,1]), Term(2, m[3])], Poly[Term(5,mp[2][5])], Poly[Term(3,mp[1,2][3,4])]] :: [Poly Rational Revlex]; c3 = [Poly[Term(3, mp[3][11]), Term(4, mp[1][2])], Poly[Term(3, mp[3][10])], Poly [Term(5,mp[1,2,3][1,0,7])]] :: [Poly Rational Revlex]
-- calculator example
-- f1 = Poly[Term(1,mp[2][2]), Term(-1, m[1,1]), Term(-1, m[0])] :: Poly Rational Revlex; f2 = Poly[Term(1,m[2]), Term(-2,mp[3][1])] :: Poly Rational Revlex; f3 = Poly[Term(1,mp[3][2]), Term(-1,m[1,1]), Term(1,m[0])] :: Poly Rational Revlex
-- c1 = [Poly[Term(1,m[6])], Poly[Term(4,m[3])], Poly[Term(6,m[5])], Poly[Term(10,m[3])]] :: [Poly Rational Revlex]; c2 = [Poly[Term(1, mp[1,2][2,1]), Term(2, m[3])], Poly[Term(5,mp[2][5])], Poly[Term(3,mp[1,2][1,1])]] :: [Poly Rational Revlex]; c3 = [Poly[Term(3, mp[3][2]), Term(4, mp[1][2])], Poly[Term(3, mp[3][1])]] :: [Poly Rational Revlex]; ps = concat $ c1:c2:c3:[]

f1 =
  Poly [Term (1, mp [2] [2]), Term (-1, m [1, 1]), Term (-1, m [0])] :: Poly Rational Revlex

f2 = Poly [Term (1, m [2]), Term (-2, mp [3] [1])] :: Poly Rational Revlex

f3 =
  Poly [Term (1, mp [3] [2]), Term (-1, m [1, 1]), Term (1, m [0])] :: Poly Rational Revlex
pallR = [f1,f2,f3]

-----------------------------------------------------------------------------------------
s1 =
  Poly [Term (1, mp' [2] [2]), Term(-1, m' [1, 1]), Term (-1, m' [0])] :: Poly Rational Revlex

s2 = Poly [Term (1, m' [2]), Term (-2, mp' [3] [1])] :: Poly Rational Revlex

s3 =
  Poly [Term (1, mp' [3] [2]), Term (-1, m' [1, 1]), Term(1, m' [0])] :: Poly Rational Revlex
pallL = [s1,s2,s3]

f4 = Poly[Term(3,m[1]), Term(-1, mp[2][1]), Term(-7, m[0])] :: Poly Rational Revlex
f5 = Poly[Term(2,m[1]), Term(3, mp[2][1]), Term(-1, m[0])]:: Poly Rational Revlex
pall1= [f4,f5]

f6 =Poly[Term(1,m[1,1]), Term(1, m[1]), Term(1, mp[2][1])] :: Poly Rational Revlex
f7 =Poly[Term(1,m[1,2]), Term(1, m[1]), Term(1, mp[2][1])] :: Poly Rational Revlex

pall2 = [f6,f7]

-- http://symbolicdata.org/XMLResources/IntPS/Behnke.xml
a1 = Poly [Term(1,m[7]), Term(-1,mp[2][7])] :: Poly Rational Revlex
a2 = Poly [Term(1,mp[2][7]), Term(-1,mp[3][7])] :: Poly Rational Revlex
a3 = Poly [Term(1,mp[3][7]), Term(-1,mp[4][7])] :: Poly Rational Revlex
a4 = Poly [Term(1,mp[4][7]), Term(-1, mp[5][7])] :: Poly Rational Revlex
a5 = Poly [Term(1,m[6,1]), Term(1, mp[2,3][6,1]),Term(1, mp[3,4][6,1]), Term(1, mp[4,5][6,1]), Term(1, mp[1,5][1,6]) ] :: Poly Rational Revlex
ps2= a1 : a2: a3: a4: a5: []

ps = [Poly[Term(1,m[2])]] :: [Poly Rational Revlex]
ps1 = [Poly[Term(1,m[2])]] :: [Poly Rational Revlex]
