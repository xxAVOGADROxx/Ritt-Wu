{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Polynomial.Wu (
PS(..),
-- -- *Division
-- psBsPS,
-- psBsSP,
-- --par
psBsUParP,
psBsUParS,
-- --seq
-- psBsSeqPS,
-- psBsSeqSP,

-- -- pure IO
-- --psBsSParP,
-- --psBsUParPUpio,

-- -- * char Set
-- charSetNormalSP,
-- charSetNormalPS,

charSetMSP,
charSetMPS,

-- charSetMSeqSP,
-- charSetMSeqPS,


t,
tp,
parps

-- basicSet
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
import Foreign.Marshal.Unsafe
import System.IO.Unsafe
import Control.DeepSeq
import GHC.Generics (Generic, Generic1)

data  Idp t ord = Idp !(Poly t ord) !Int deriving (Eq, Generic)
newtype PS t ord = PS (Array N Ix1 (Idp t ord )) deriving (Eq,Generic, NFData)

instance (NFData t) =>  NFData (Idp t ord) where 
  rnf (Idp t ord) = rnf t `deepseq` rnf ord `deepseq` ()

-- polynomial id
idp :: Poly t ord  -> Idp t ord
idp xs = Idp xs 0


-- polynomials id to parallel polynomial set
ps :: (NFData t) => [Idp t ord] -> PS t ord
ps xs = PS $ A.fromList (ParN 2) xs

--wrapper to write terms
t :: k -> [Int] -> Term k ord
t k mon = Term k $ m mon

--wrapper to write terms with a givne position
tp :: k -> [Int] -> [Int] -> Term k ord
tp k mon mon' = Term k $ mp mon mon'

--parallel forma of a polynomial set 
parps :: (NFData t) =>[Poly t ord] -> PS t ord
parps xs = ps $ P.map idp xs

-- how write polynomials
p1 =  p [t 3 [1,2], tp 4 [2,4][1,2], t 4 [1,2] ] :: Poly Rational Revlex
p2 =  p [t 3 [2], t 4 [1] ] :: Poly Rational Revlex
p3 =  p [t 3 [5,6], t 4 [1] ] :: Poly Rational Revlex
p4 =  p [t 3 [2,1,2], t 4 [1] ] :: Poly Rational Revlex
p5 =  p [t 3 [1], t 6 [2] ] :: Poly Rational Revlex
p6 =  p [t 3 [1,2,3], t 6 [2] ] :: Poly Rational Revlex
example = parps [p1,p2,p3,p4,p5,p6]


----------------------------------------------------------------------------------------------------
instance (NFData t, Ord t, Num t, Show t) => Show (PS t ord) where
  show xs = concat $ showPoly xs

showPoly :: (NFData t, Ord t, Show t, Num t) => PS t ord -> [String]
showPoly (PS xs) = conv
  where
    conv = L.map (\ x -> showIdp x ++ "\n") (A.toList xs)
    --f (Idp poly id) = show poly ++ "<<" ++show id ++ ">>,"
----------------------------------------------------------------------------------------------------
setClassPs :: (NFData t, Eq t) =>PS t ord -> PS t ord
setClassPs =  sortGrupPolyClass . searchPolyClass

searchPolyClass :: (NFData t )=> PS t ord  -> PS t ord
searchPolyClass (PS ps) = PS $ A.computeAs N (A.map f ps)
  where
    f (Idp poly id) = Idp poly (class' poly)


sortGrupPolyClass :: (NFData t, Eq t) => PS t ord -> PS t ord
sortGrupPolyClass (PS xs) = PS conversion
  where
    conversion =  A.quicksort xs 

instance (NFData t, Eq t) => Ord (Idp t ord) where
  compare (Idp poly id)(Idp poly' id') = compare id id'


-- sucesive pseudo division of a polynomial and a polynomial set (SPOLY)
sspoly :: (NFData t, Fractional t, Num t, Eq t, Ord t) => Idp t Revlex  -> PS t Revlex  -> Idp t Revlex
sspoly (Idp pol id)(PS xs) = Idp (sspoly' pol (P.map f (A.toList xs))) 0
  where
    f (Idp pol' id') = pol'

sspoly' :: (NFData t, Fractional t, Ord t, Num t, Eq t) => Poly t Revlex -> [Poly t Revlex] -> Poly t Revlex
sspoly' rm [] = rm
sspoly' rm (b:bs)
  | a /= p[] = sspoly' a bs
  | otherwise = p[]
  where
    a = spoly rm b

-- sucesive pseudo division of a polynomial and a polynomial set
sprem :: (NFData t, Num t, Eq t) => Idp t Revlex  -> PS t Revlex  -> Idp t Revlex
sprem (Idp pol id)(PS xs) = Idp (sprem' pol (P.map f (A.toList xs))) 0
  where
    f (Idp pol' id') = pol'

sprem' :: (NFData t, Num t, Eq t) => Poly t Revlex -> [Poly t Revlex] -> Poly t Revlex
sprem' rm  [] = rm
sprem' rm (b:bs)
  | a /= p[] = sprem' a bs
  | otherwise = p[]
  where
    a = prem rm b

-- -- reducction of two list of  polynomials
red :: (NFData t, Eq t) => PS t ord -> PS t ord -> PS t ord
red (PS poly)(PS poly') = ps $ on (P.foldl (flip delete)) f poly' poly
  where
    f = A.toList


-- -- Monad IO division
-- psBsUParPUpio :: (Num t, Eq t) =>[Poly t Revlex] -> [Poly t Revlex] -> [Poly t Revlex]
-- psBsUParPUpio ps bs =  unsafePerformIO ( traverseConcurrently Par (\p -> p `deepseq` pure p) [sprem  x bs | x <- ps] )

-----------------------------------------------------------------------------
-- take a Basic Set BS from a PS (but only lower rank )
-- basicSet' :: (Num t, Eq t) => [[Poly t Revlex]] -> [Poly t Revlex]
-- basicSet' xs = searchLowerRank xs []
------------------------------------------------
-- -- a function that search a poly with lower rank and reduced x_i
-- search Poly Lower Rank and Reduced
groupClass ::(NFData t, Num t, Eq t)=> PS t ord -> [[Idp t ord]]
groupClass (PS poly) = groupBy (on (==) f) $ A.toList poly
  where
    f (Idp pol id) = id

sPLRR :: (NFData t, Num t, Eq t) => [[Idp t Revlex]] -> [Idp t Revlex] -> PS t Revlex 
sPLRR (x:xs) [] = sPLRR xs [head $ lDPolys x]
sPLRR [] xp = ps xp
sPLRR xs xp 
  | f (head xp) == 0 = error "Contradictory AS There is (are) constant inside the polynomial set"
  | h  /= z = sPLRR (tail xs) (h : xp)
  | otherwise = sPLRR (tail xs) xp
  where
    f(Idp poly id) = class' poly
    g = lDPolys . head
    h =  reducedP (g xs) (head xp)
    z = idp (p[])

-- check if a given polynomial is reduced with respect to a set
reducedP :: (NFData t, Num t, Eq t)=> [Idp t Revlex] -> Idp t Revlex -> Idp t Revlex
reducedP [] _ = idp (p[])
reducedP (x:xs) z 
  | isReduced x z  = x
  | otherwise = reducedP xs z
-----------------------------------------------------------------
-- izq the entering polynomial, right the forming basic set
-- give two polyomials a b check if a is reduced to b
isReduced :: (NFData t, Eq t, Num t) => Idp t Revlex -> Idp t Revlex -> Bool
isReduced (Idp poly id) (Idp poly' id')
 -- | degP poly (class' poly') P.< degP poly' (class' poly') = True
  | prem poly poly' == poly = True
  | otherwise = False
-- -----------------------------------------------------------------
-- compare the degree of two polynomials in a given variable
degP ::(NFData t ) => Poly t Revlex -> Int -> Int
degP ( Poly poly) cls = A.maximum' .  A.map h . A.computeAs N . A.filterS (\x -> f x >= cls) $  poly
  where
    f(Term k mon) = A.elemsCount (g mon)
    g(Mon m) = m
    h (Term k mon) = (g mon) ! (cls P.-1)

----------------------------------------------------------------------------------------------------
--search the polynomial (S) with lower degree in a list of polynomials with the same class
lDPolys :: (NFData t) => [Idp t Revlex]-> [Idp t Revlex]
lDPolys xs = head $ groupBy (on (==) i) (sortBy (on compare i) f) 
  where
    f = L.map g xs
    g (Idp poly id) = Idp poly (ld poly)
    i (Idp poly id) = id

-----------------------------------------------------------------------------
quitEmptyPoly :: (NFData t, Eq t) =>  PS t Revlex -> PS t Revlex
quitEmptyPoly (PS pol)= PS $ A.computeAs N $ ( A.filterS (\x -> x /= f) pol)
  where
    f = idp $ p[]
----------------------------------------------------------------------------

instance (NFData t, Ord t, Num t, Show t) => Show (Idp t ord) where
  show xs = showIdp xs

showIdp ::(NFData t, Ord t, Num t, Show t)=> Idp t ord -> String
showIdp (Idp poly id) =  show poly -- ++ "<<" ++show id ++ ">>"
----------------------------------------------------------------
-- take a basic set using the classification class
basicSet :: (NFData t, Eq t, Num t) => PS t Revlex -> PS t Revlex
basicSet xs =  sPLRR classify []
  where
    classify = groupClass . setClassPs $ xs


-- -- ////////////////////////////////////////////////////////////////////////////////////////////////////
-- --begin: char Set Normal PS---------------------------------------------------------------------------
-- charSetNormalPS :: [Poly Rational Revlex] -> [Poly Rational Revlex]
-- charSetNormalPS ps
--   | bs == ps = ps
--   | rs == ps' = bs
--   | rs /= [] = charSetNormalPS (rs++ bs)
--   | otherwise = bs
--   where
--     rs = bsDividePsPS ps' bs
--     bs = basicSet ps
--     ps' = red bs ps
-- ---------------------------------------------------------------------------------------------------
-- bsDividePsPS ::(Num t, Eq t) =>[Poly t Revlex] -> [Poly t Revlex] -> [Poly t Revlex]
-- bsDividePsPS ps' bs = quitEmptyPoly (psBsPS ps' bs)
-- ----------------------------------------------------------------------------------------------------
-- psBsPS :: (Num t, Eq t)=>[Poly t Revlex] -> [Poly t Revlex] -> [Poly t Revlex]
-- psBsPS ps bs = [ force (sprem x bs) | x <- ps]

-- --end: char Set Normal PS--------------------------------------------------------------------------
-- --begin: char Set Normal SP-------------------------------------------------------------------------
-- charSetNormalSP :: [Poly Rational Revlex] -> [Poly Rational Revlex]
-- charSetNormalSP ps
--   | bs == ps = ps
--   | rs == ps' = bs
--   | rs /= [] = charSetNormalSP (rs++ bs)
--   | otherwise = bs
--   where
--     rs = bsDividePsSP ps' bs
--     bs = basicSet ps
--     ps'
--       = red bs ps
-- ----------------------------------------------------------------------------------------------------
-- bsDividePsSP ::(Fractional t, Ord t, Num t, Eq t) =>[Poly t Revlex] -> [Poly t Revlex] -> [Poly t Revlex]
-- bsDividePsSP ps' bs = quitEmptyPoly (psBsSP ps' bs)
-- ----------------------------------------------------------------------------------------------------
-- psBsSP :: (Fractional t, Ord t, Num t, Eq t)=>[Poly t Revlex] -> [Poly t Revlex] -> [Poly t Revlex]
-- psBsSP ps bs = [ force (sspoly x bs) | x <- ps]

-- ----- *****************************-----------------------
-- begin: char Set parallel division PS
--characteristic set in parallel computation using pseudo remainder
charSetMPS :: (NFData t, Eq t, Num t) => PS t Revlex -> PS t Revlex 
charSetMPS ps
  | bs == ps = ps
  | rs == ps' = bs
  | rs /= parps[] = charSetMPS (clean (PS $ A.computeAs N $ on (A.append' 1) f rs bs))
  | otherwise = bs
  where
    rs = clean $ bsDividePsMPS ps' bs
    bs = clean $ basicSet ps
    ps' = red bs ps
    f(PS poly) = poly;
    clean (PS poly) = PS $ A.computeAs N (A.map g poly);
    g (Idp poly id)= Idp poly 0

-- orden' :: PS t ord -> PS t ord
-- orden' (PS polys) = A.map (f)  polys
--   where
--     f (Idp poly id) = g poly ()

-- f(PS poly) = poly; clean (PS poly) = PS $ A.computeAs N (A.map g poly); g (Idp poly id)= Idp poly 0

bsDividePsMPS ::(NFData t,  Num t, Eq t) => PS t Revlex  -> PS t Revlex -> PS t Revlex
bsDividePsMPS ps' bs = quitEmptyPoly (psBsUParP ps' bs)
-- ----------------------------------------------------------------------------------------------------
psBsUParP :: (NFData t, Num t, Eq t) => PS t Revlex  -> PS t Revlex -> PS t Revlex
psBsUParP (PS ps) (bs) = PS $ A.computeAs N $ A.map (`sprem` bs) ps
-- --psBsUParP ps bs =  unsafePerformIO ( traverseConcurrently (ParN 4) (\p -> p `deepseq` pure p) [sprem  x bs | x <- ps] )
-- psBsUParP ps bs =  unsafePerformIO ( traverseConcurrently Par (pure $!) [sprem  x bs | x <- ps] )
-- --psBsUParP ps bs =  unsafePerformIO ( traverseConcurrently (ParN $ fromIntegral (length ps)) (\p -> myThreadId >>= print >> pure p) [sprem  x bs | x <- ps] )
-- laia ps bs =  unsafePerformIO ( traverseConcurrently (Par) (\p -> myThreadId >>= print >> pure p) [div  x bs | x <- ps] )
-- -- ahora me pide el nfdata pra poly (sin siquiera haberlo puesto en el bench)
--end : char Ser parallel division PS--------------------------------------------------------------------------------------------------

--begin: characteristic set Parallel division Spoly
charSetMSP :: (NFData t, Fractional t , Ord t, Eq t, Num t) => PS t Revlex -> PS t Revlex 
charSetMSP ps
  | bs == ps = ps
  | rs == ps' = bs
  | rs /= parps[] = charSetMSP (clean (PS $ A.computeAs N $ on (A.append' 1) f rs bs))
  | otherwise = bs
  where
    rs = clean $ bsDividePsMSP ps' bs
    bs = clean $ basicSet ps
    ps' = red bs ps
    f(PS poly) = poly;
    clean (PS poly) = PS $ A.computeAs N (A.map g poly);
    g (Idp poly id)= Idp poly 0

bsDividePsMSP ::(NFData t, Fractional t, Ord t, Num t, Eq t) => PS t Revlex  -> PS t Revlex -> PS t Revlex
bsDividePsMSP ps' bs = quitEmptyPoly (psBsUParS ps' bs)

psBsUParS :: (NFData t, Fractional t, Num t, Eq t, Ord t) =>PS t Revlex  -> PS t Revlex -> PS t Revlex
psBsUParS (PS ps) (bs) = PS $ A.computeAs N $ A.map (`sspoly` bs) ps

----- *****************************-----------------------
-- -- charSet sequetial computation
-- --begin: char set sequential PS---------------------------------------------------------------------
-- charSetMSeqPS :: [Poly Rational Revlex] -> [Poly Rational Revlex]
-- charSetMSeqPS ps
--   | bs == ps = ps
--   | rs == ps' = bs
--   | rs /= [] = charSetMSeqPS (rs ++ bs)
--   | otherwise = bs
--   where
--     rs = bsDividePsMseqPS ps' bs
--     bs = basicSet ps
--     ps' = red bs ps
-- -------------------------------------------------------------------------------------------------
-- bsDividePsMseqPS ::(Fractional t, Ord t, Num t, Eq t) =>[Poly t Revlex] -> [Poly t Revlex] -> [Poly t Revlex]
-- bsDividePsMseqPS ps' bs = quitEmptyPoly (psBsSeqPS ps' bs )
-- ---------------------------------------------------------------------------------------------------
-- psBsSeqPS :: (Num t, Eq t)=>[Poly t Revlex] -> [Poly t Revlex] -> [Poly t Revlex]
-- psBsSeqPS ps bs =  unsafePerformIO ( traverseConcurrently Seq (\p -> p `deepseq` pure p) [sprem  x bs | x <- ps] )
-- --end: char set sequential PS-------------------------------------------------------------
-- --begin: char set sequential SP--------------------------------------------------------------------------------------------------
-- charSetMSeqSP :: [Poly Rational Revlex] -> [Poly Rational Revlex]
-- charSetMSeqSP ps
--   | bs == ps = ps
--   | rs == ps' = bs
--   | rs /= [] = charSetMSeqSP (rs++ bs)
--   | otherwise = bs
--   where
--     rs = bsDividePsSP ps' bs
--     bs = basicSet ps
--     ps' = red bs ps
-- -----------------------------------------------------------------------------
-- bsDividePsMseqSP ::(Fractional t, Ord t, Num t, Eq t) =>[Poly t Revlex] -> [Poly t Revlex] -> [Poly t Revlex]
-- bsDividePsMseqSP ps' bs = quitEmptyPoly (psBsSeqSP ps' bs)
-- ----------------------------------------------------------------------------------------------------
-- psBsSeqSP :: (Fractional t, Ord t, Num t, Eq t)=>[Poly t Revlex] -> [Poly t Revlex] -> [Poly t Revlex]
-- psBsSeqSP ps bs =  unsafePerformIO ( traverseConcurrently Seq (\p -> p `deepseq` pure p) [sspoly  x bs | x <- ps] )
-- --end: charset sequential SP
-- -- **********************************************************************

-- -- parallel division pure IO
-- psBsSParP ::  [Poly Rational Revlex] -> [Poly Rational Revlex] -> IO[Poly Rational Revlex]
-- psBsSParP ps bs = traverseConcurrently Par (\p -> p `deepseq` pure p) [sprem  x bs | x <- ps] :: IO [Poly Rational Revlex]

-- psBsSParS ::  [Poly Rational Revlex] -> [Poly Rational Revlex] -> IO[Poly Rational Revlex]
-- psBsSParS ps bs = traverseConcurrently Par (\p -> p `deepseq` pure p) [sspoly  x bs | x <- ps] :: IO [Poly Rational Revlex]

-- -- sequential division pure IO 
-- psBsSSeqP ::  [Poly Rational Revlex] -> [Poly Rational Revlex] -> IO[Poly Rational Revlex]
-- psBsSSeqP ps bs = traverseConcurrently Seq (\p -> p `deepseq` pure p) [sprem  x bs | x <- ps] :: IO [Poly Rational Revlex]

-- psBsSSeqS ::  [Poly Rational Revlex] -> [Poly Rational Revlex] -> IO[Poly Rational Revlex]
-- psBsSSeqS ps bs = traverseConcurrently Seq (\p -> p `deepseq` pure p) [sspoly  x bs | x <- ps] :: IO [Poly Rational Revlex]

-- psBsSParn1P ::  [Poly Rational Revlex] -> [Poly Rational Revlex] -> IO[Poly Rational Revlex]
-- psBsSParn1P ps bs = traverseConcurrently (ParN 1) (\p -> p `deepseq` pure p) [sprem  x bs | x <- ps] :: IO [Poly Rational Revlex]

-- instance NFData (IO[Poly Rational Revlex]) where
--   rnf x = seq x ()
-- instance NFData (Poly t Revlex) where
--   rnf x = seq x ()
-- -- psByTfPM :: (Num t, Eq t) =>  [Poly t Revlex] -> [Poly t Revlex]-> IO [Poly t Revlex]
-- -- psByTfPM ps bs = withScheduler Par $ \x -> do
-- --   p <- ps
-- --   scheduleWork x $ pure (sprem p bs )

-- --psByTfP :: (Num t, Eq t) => [Poly t Revlex] -> [Poly t Revlex]-> IO [[Poly t Revlex]]
-- --psByTfP ps bs = withScheduler Par $ \x -> scheduleWork x $ pure ([(sprem x) bs | x <- ps]) -- paralelisa?

-- --psByTfPR :: (Num t, Eq t) => [Poly t Revlex] -> [Poly t Revlex]-> IO [[Poly t Revlex]]
-- --psByTfPR ps bs = withScheduler Par $ \x -> replicateM (length ps) $ scheduleWork x $ pure ([(sprem x) bs | x <- ps]) -- replica one single work

-- --psByTfPS :: (Num t, Eq t) => [Poly t Revlex] -> [Poly t Revlex]-> IO [Poly t Revlex]
-- --psByTfPS ps bs = withScheduler Par $ \x -> do scheduleWork x $ pure ( sprem (ps !! 0) bs )
-- --                                              scheduleWork x $ pure ( sprem (ps !! 1) bs ) -- there is no way we can know the length of a set
-- -- como crear funciones de acuerdo al input

-- -- laia :: Int -> Int -> IO [Int]
-- -- laia a b= withScheduler Par $ \x -> replicateM_ a $ scheduleWork  x  $ pure (L.foldl' (P.+) 0 [0 .. b]) -- scheduleId x

-- -- laia' :: Int -> Int -> IO[Int]
-- -- laia' a b = withScheduler Par $ \x -> do replicateM a $ scheduleWork  x  $ pure (L.foldl' (P.+) 0 [0 .. b]) -- scheduleId x
-- -- -- scheduleId = (`scheduleWorkId` (\ i -> threadDelay 1000000 >> print i >> pure (L.foldl' (P.+) 0 [0 .. 100000] )))

-- -- laia'' :: Int -> IO [Int]
-- -- laia'' a =
-- --   withScheduler Par $ \x -> do
-- --     scheduleWork x $ pure (L.foldl' (P.+) 0 [0 .. div a 2])
-- --     scheduleWork x $ pure (L.foldl' (P.+) 0 [div a 2  .. a])
-- ---------------- ************** -----------------------------
-- -----------------------------------------------------------------------------
-- --red [] xp = xp
-- --red (x:xs) xp = red xs (delete x xp)
-- -----------------------------------------------------------------------------
-- -- graphics calculator examples
-- -- p1 = Poly [Term(1,mp[2][2]), Term(-1,m[1,1]), Term(-1,m[0])] :: Poly Rational Revlex
-- -- p2 = Poly [Term(1,m[2]), Term(-2,mp[3][1])]:: Poly Rational Revlex
-- -- p3 = Poly [Term(1,mp[3][2]), Term(-1,m[1,1]), Term(1,m[0])] :: Poly Rational Revlex
-- -- p4 = Poly [Term3,mp[2][5]] :: Poly Rational Revlex
-- -- ps = [Poly [Term(1,mp[2][2]), Term(-1,m[1,1]), Term(-1,m[0])] :: Poly Rational Revlex, Poly [Term(1,m[2]), Term(-2,mp[3][1])]:: Poly Rational Revlex, Poly [Term(1,mp[3][2]), Term(-1,m[1,1]), Term(1,m[0])] :: Poly Rational Revlex]
-- --Poly [Term(3,mp[2][5])] :: Poly Rational Revlex]
-- -- ps1 = [Poly [Term(1,mp[2][2]), Term(-1,m[1,1]), Term(-1,m[0])] :: Poly Rational Revlex, Poly [Term(1,m[2]), Term(-2,mp[3][1])]:: Poly Rational Revlex, Poly [Term(1,mp[3][2]), Term(-1,m[1,1]), Term(1,m[0])] :: Poly Rational Revlex, Poly [Term(3,mp[2][5])] :: Poly Rational Revlex]

-- -- Buchberger
-- -- f1 = Poly [Term(1,mp[1,4][1,2]), Term(1,mp[4][2]), Term(-1,mp[1,2,4][1,1,1]), Term(-1,mp[2,4][1,1]), Term(1,m[1,1]), Term(3, mp[2][1])] :: Poly Rational Revlex; f2 = Poly [Term(1,mp[1,4][1,1]), Term(1,mp[3][1]), Term(-1,m[1,1])] :: Poly Rational Revlex; f3 = Poly [Term(1,mp[3,4][1,1]), Term(-2,mp[2][2]),Term(-1,m[1,1]),Term(-1,m[0])] :: Poly Rational Revlex; f4 = prem f1 f2
-- -- asc
-- -- f1 = Poly [Term(1,m[5])] :: Poly Rational Revlex; f2 = Poly [Term(1,m[6]), Term(1,mp[2][1])] :: Poly Rational Revlex; f4 = Poly [Term(1,m[1]),Term(1,mp[2][3])]; f3 = Poly[Term(1,m[2])]  :: Poly Rational Revlex
-- -- lower = [Poly[Term(1,m[6])], Poly[Term(4,m[3])], Poly[Term(6,m[5])], Poly[Term(10,m[3])], Poly[Term(1, mp[2][3]), Term(2, m[3])], Poly[Term(5,mp[2][5])], Poly[Term(3, mp[3][2]), Term(4, mp[1][2])], Poly[Term(3, mp[3][1])]]

-- -- c1 = [Poly[Term(1,m[6])], Poly[Term(4,m[3])], Poly[Term(6,m[5])], Poly[Term(10,m[3])] :: Poly Rational Revlex; c2 = Poly[Term(1, mp[1,2][2,1]), Term(2, m[3])], Poly[Term(5,mp[2][5])] :: Poly Rational Revlex; c3 =  Poly[Term(3, mp[3][2]), Term(4, mp[1][2])], Poly[Term(3, mp[3][1])]] :: Poly Rational Revlex
-- -- c1 = [Poly[Term(1,m[6])], Poly[Term(4,m[5])], Poly[Term(6,m[7])], Poly[Term(10,m[3])]] :: [Poly Rational Revlex]; c2 = [Poly[Term(4, mp[1,2][2,1])],Poly[Term(1, mp[1,2][2,1]), Term(2, m[3])], Poly[Term(5,mp[2][5])], Poly[Term(3,mp[1,2][3,4])]] :: [Poly Rational Revlex]; c3 = [Poly[Term(3, mp[3][11]), Term(4, mp[1][2])], Poly[Term(3, mp[3][10])], Poly [Term(5,mp[1,2,3][1,0,7])]] :: [Poly Rational Revlex]
-- -- calculator example
-- -- f1 = Poly[Term(1,mp[2][2]), Term(-1, m[1,1]), Term(-1, m[0])] :: Poly Rational Revlex; f2 = Poly[Term(1,m[2]), Term(-2,mp[3][1])] :: Poly Rational Revlex; f3 = Poly[Term(1,mp[3][2]), Term(-1,m[1,1]), Term(1,m[0])] :: Poly Rational Revlex
-- -- c1 = [Poly[Term(1,m[6])], Poly[Term(4,m[3])], Poly[Term(6,m[5])], Poly[Term(10,m[3])]] :: [Poly Rational Revlex]; c2 = [Poly[Term(1, mp[1,2][2,1]), Term(2, m[3])], Poly[Term(5,mp[2][5])], Poly[Term(3,mp[1,2][1,1])]] :: [Poly Rational Revlex]; c3 = [Poly[Term(3, mp[3][2]), Term(4, mp[1][2])], Poly[Term(3, mp[3][1])]] :: [Poly Rational Revlex]; ps = concat $ c1:c2:c3:[]



-- -----------------------------------------------------------------------------------------
-- -- s1 =
-- --   Poly [Term (1, mp' [2] [2]), Term(-1, m' [1, 1]), Term (-1, m' [0])] :: Poly Rational Revlex

-- -- s2 = Poly [Term (1, m' [2]), Term (-2, mp' [3] [1])] :: Poly Rational Revlex

-- -- s3 =
-- --   Poly [Term (1, mp' [3] [2]), Term (-1, m' [1, 1]), Term(1, m' [0])] :: Poly Rational Revlex
-- -- pallL = [s1,s2,s3]

f4 = p[t 3 [1], tp (-1) [2][1], t (-7) []] :: Poly Rational Revlex
f5 = p[t 2 [1], tp 3 [2][1], t (-1) []]:: Poly Rational Revlex
pall1= [f4,f5]

f6 =p[t 1 [1,1], t 1 [1], tp 1 [2][1]] :: Poly Rational Revlex
f7 =p[t 1 [1,2], t 1 [1], tp 1 [2][1]] :: Poly Rational Revlex

pall2 = parps  [f6,f7]

-- -- http://symbolicdata.org/XMLResources/IntPS/Behnke.xml
-- a1 = Poly [Term(1,m[7]), Term(-1,mp[2][7])] :: Poly Rational Revlex
-- a2 = Poly [Term(1,mp[2][7]), Term(-1,mp[3][7])] :: Poly Rational Revlex
-- a3 = Poly [Term(1,mp[3][7]), Term(-1,mp[4][7])] :: Poly Rational Revlex
-- a4 = Poly [Term(1,mp[4][7]), Term(-1, mp[5][7])] :: Poly Rational Revlex
-- -- a5 = Poly [Term(1,m[6,1]), Term(1, mp[2,3][6,1]),Term(1, mp[3,4][6,1]), Term(1, mp[4,5][6,1]), Term(1, mp[1,5][1,6]) ] :: Poly Rational Revlex
-- ps2= a1 : a2: a3: a4: a5: []

-- ps = [Poly[Term(1,m[2])]] :: [Poly Rational Revlex]
-- ps1 = [Poly[Term(1,m[2])]] :: [Poly Rational Revlex]

-- t1 = Poly [Term(1, m[4])]:: Poly Rational Revlex
-- --t1 = Poly [Term(1, m[4]), Term(1,m[])]:: Poly Rational Revlex
-- t2 = Poly [Term(1,m[2])]::Poly Rational Revlex
-- t3 = Poly [Term(1,mp[2][2]),Term(1, m[2]) ]::Poly Rational Revlex
-- --t3 = Poly [Term(1,mp[1,2][4,2])]::Poly Rational Revlex
-- t4 = Poly [Term(1,mp[3][2])]::Poly Rational Revlex
-- pt = [t1,t2,t3,t4]
-- ********************************************************************************************************************************************************************************************************
f1 = p [tp 1 [2] [2], t (-1) [1, 1], t (-1) []] :: Poly Rational Revlex
f2 = p [t 1 [2], tp (-2) [3] [1]] :: Poly Rational Revlex
f3 = p [tp 1 [3] [2], t (-1) [1, 1], t 1 []] :: Poly Rational Revlex
ps1 = parps [f2,f1, f3]
----------------------------------------------------------------------------------------------------
a6 =
  p [tp 1 [2] [2], tp 1 [3] [2], tp 1 [4] [2], t (-1) [2]] :: Poly Rational Revlex
a7 =
  p [tp 1 [2, 3] [1, 1], tp 1 [4] [2], t (-1) []] :: Poly Rational Revlex
a8 =
  p
    [ tp 1 [2, 3, 4] [1, 1, 1]
    , tp (-1) [2] [2]
    , tp (-1) [3] [2]
    , tp (-1) [4] [1]
    , t 1 []
    ] :: Poly Rational Revlex
ps2' = a6 : a7 : a8 : []
ps2 = parps ps2'
----------------------------------------------------------------------------------------------------
a9 =
  p
    [ tp 1 [1, 10] [2, 1]
    , tp 2 [1, 2, 11] [1, 1, 1]
    , tp 3 [2, 12] [2, 1]
    , tp 1 [1, 4] [1, 1]
    , tp 2 [2, 5] [1, 1]
    , tp 1 [7] [1]
    ] :: Poly Rational Revlex
a10 =
  p
    [ tp 3 [1, 9] [2, 1]
    , tp 2 [1, 2, 10] [1, 1, 1]
    , tp 1 [2, 11] [2, 1]
    , tp 2 [1, 3] [1, 1]
    , tp 1 [2, 4] [1, 1]
    , tp 1 [6] [1]
    ] :: Poly Rational Revlex
a11 =
  p
    [ tp 1 [1, 9] [3, 1]
    , tp 1 [1, 2, 10] [2, 1, 1]
    , tp 1 [1, 2, 11] [1, 2, 1]
    , tp 1 [2, 12] [3, 1]
    , tp 1 [1, 3] [2, 1]
    , tp 1 [1, 2, 4] [1, 1, 1]
    , tp 1 [2, 5] [2, 1]
    , tp 1 [1, 6] [1, 1]
    , tp 1 [2, 7] [1, 1]
    , tp 1 [8] [1]
    ] :: Poly Rational Revlex
ps3' = a9 : a10 : a11 : []
ps3 = parps ps3'
----------------------------------------------------------------------------------------------------
a12 = p [ tp 1 [1,2,3][2,4,1], tp 1 [1,2][2,4], tp 2 [1,2,3][2,2,1], tp 6 [1,2][2,2], tp (-4) [1,2][1,3], tp 1 [2,3][4,1], tp (-1) [2][4], tp 1 [1,3][2,1], tp 1 [1][2], tp 4 [1,2][1,1], tp 2 [2,3][2,1], tp (-6) [2][2], tp 1 [3][1], t (-1) []
        ] :: Poly Rational Revlex
a13 = p [ tp 1 [1,2,3][8,3,1], tp 1 [1,2,3][8,1,1], tp 4 [1,2,3][6,3,1], tp (-8) [1,2][6,3], tp 8 [1,2][5,4], tp 4 [1,2,3][6,1,1], tp 8 [1,2][6,1], tp (-48) [1,2][5,2], tp 6 [1,2,3][4,3,1], tp 48 [1,2][4,3], tp (-8) [1,2][3,4], tp 8 [1][5], tp 6 [1,2,3][4,1,1], tp (-48) [1,2][4,1], tp (48) [1,2][3,2], tp 4 [1,2,3][2,3,1], tp (-8) [1,2][2,3], tp (-8) [1][3], tp 4 [1,2,3][2,1,1], tp (8) [1,2][2,1], tp (1) [2,3][3,1], tp 1 [2,3][1,1]
         ] :: Poly Rational Revlex
ps4 = parps [a12,a13]
----------------------------------------------------------------------------------------------------
a14 = p[ tp 35 [2][1], tp 40 [3][1], tp 25 [4][1], tp (-27) [5][1]] :: Poly Rational Revlex
a15 = p[ tp 45 [2][1], tp 35 [5][1], tp (-165) [6][1], t (-36) []] :: Poly Rational Revlex
a16 = p[ tp (-11)[5,6][1,1], tp (3)[6][2],tp (99) [1][1]  ] :: Poly Rational Revlex
a17 = p[ tp 25 [2,5][1,1], tp (-165)[6][2], tp (15) [1][1], tp 30 [3][1], tp (-18) [4][1] ] :: Poly Rational Revlex
a18 = p[ tp 15 [2,4][1,1], tp 20 [3,5][1,1], tp (-9) [1][1]] :: Poly Rational Revlex
a19 = p[ tp (-11)[6][3], tp 1 [1,2][1,1], tp 2 [3,4][1,1] ]  :: Poly Rational Revlex
ps5 = parps [a14,a15,a16,a17,a18,a19]
----------------------------------------------------------------------------------------------------
a20 = p[t  (-4) [1], tp 1 [2][1], tp 1 [3][1], tp 1 [4][1]] :: Poly Rational Revlex
a21 = p[tp (-4) [1][2], tp 1 [2][2], tp 1 [3][2], tp 1 [4][2], tp 4 [1][1], tp 1 [2][1], tp 1 [3][1], tp 1 [4][1], t (-3) []] :: Poly Rational Revlex
a22 = p[tp (5) [1][3], tp 4 [3,4][2,1], tp 3 [2][2], tp 2 [1,4][1,1], tp 4 [1][1], tp 1 [2][1], tp 1 [3][1], tp 2 [4][1], t (-1) []] :: Poly Rational Revlex
a23 = p[tp 5 [3][4], tp 1 [4][3], tp 16 [1][2], tp 3 [2][2], tp (-4) [4][1], t (-1) []] :: Poly Rational Revlex
ps6 = parps [a20,a21,a22,a23]
----------------------------------------------------------------------------------------------------
a24 = p[tp 1 [1][1], tp 1 [2][1], tp 1 [3][1], tp 1 [4][1], tp 1 [5][1]] :: Poly Rational Revlex
a25 = p[tp 1 [1,2][1,1], tp 1 [2,3][1,1], tp 1 [3,4][1,1], tp 1 [1,5][1,1], tp 1 [4,5][1,1]] :: Poly Rational Revlex
a26 = p[tp 1 [1,2,3][1,1,1], tp 1 [2,3,4][1,1,1], tp 1 [1,2,5][1,1,1], tp 1 [1,4,5][1,1,1], tp 1 [3,4,5][1,1,1]] :: Poly Rational Revlex
a27 = p[tp 1 [1,2,3,4][1,1,1,1], tp 1 [1,2,3,5][1,1,1,1], tp 1 [1,2,4,5][1,1,1,1], tp 1 [1,3,4,5][1,1,1,1], tp 1 [2,3,4,5][1,1,1,1]] :: Poly Rational Revlex
a28 = p[t 1 [1,1,1,1,1], t (-1) []] :: Poly Rational Revlex
ps7 = parps [a24,a25,a26, a27, a28]
----------------------------------------------------------------------------------------------------
a29 = p[tp (-2) [1,3][1,1], tp (-2) [3][2], tp (-2)[1][1], tp (8) [3][1], t (-2) []]:: Poly Rational Revlex
a30 = p[tp (-3)[1,2,3][1,1,1], tp (2) [1,3,4][1,1,1], tp (4)[3,4][2,1], tp (3)[2,3][1,1], tp 1 [1,4][1,1], tp (-7) [3,4][1,1]]:: Poly Rational Revlex
a31 = p[tp 1 [1,2][2,2], tp (-2)[1,2,4][2,1,1], t (-2) [1,1,1,1], tp 1 [1,4][2,2], tp 2 [1,3,4][1,1,2], tp 1 [3,4][2,2], tp (-2)[1,2][1,2], t (4) [1,1,1], tp 2 [1,2,4][1,1,1], tp 2 [2,3,4][1,1,1], tp (-4)[1,4][1,2], tp (-4) [3,4][1,2], tp 1 [2][2], tp 2 [1,3][1,1], tp 10 [3][2], tp (-4)[2,4][1,1], tp 4 [4][2], tp (-2)[1][1], tp (-8)[3][1], t 2 [] ]:: Poly Rational Revlex
a32 = p[t 3 [2,2], t 12 [1,1,1,1], tp (-3) [1,4][2,2], tp 6 [1,3,4][1,1,2], tp (-3) [3,4][2,2], tp (-6) [1,2][1,2], tp 12 [1,2,4][1,1,1], tp 12 [2,3,4][1,1,1], tp (-4) [1][2], tp 3 [2][2], tp 5 [3][2], tp (-12) [2,4][1,1], tp 12 [4][2], tp (-6) [3][1], t 5 [] ] :: Poly Rational Revlex
ps8 = parps [a29,a30,a31,a32]
----------------------------------------------------------------------------------------------------
-- ojito

----------------------------------------------------------------------------------------------------
a39 = p[tp 1 [2,3][1,5], t 1 [1], t (-2)[] ]:: Poly Rational Revlex
a40 = p[tp 1 [1,3][5,1], tp 1 [2][1], t (-2)[]]:: Poly Rational Revlex
a41 = p[t 1 [1,5], tp 1 [3][1], t (-2) []]:: Poly Rational Revlex
ps11 = parps [a39,a40,a41]
----------------------------------------------------------------------------------------------------
a46 = p[tp 1 [1][1], tp 1 [2][1], tp 1 [3][1], tp 1 [4][1], tp 1 [5][1], tp 1 [6][1]] :: Poly Rational Revlex
a47 = p[tp 1 [1,2][1,1], tp 1 [2,3][1,1], tp 1 [3,4][1,1], tp 1 [4,5][1,1], tp 1 [1,6][1,1], tp 1 [5,6][1,1]] :: Poly Rational Revlex
a48 = p[tp 1 [1,2,3][1,1,1], tp 1 [2,3,4][1,1,1], tp 1[3,4,5][1,1,1], tp 1 [1,2,6][1,1,1], tp 1 [1,5,6][1,1,1], tp 1 [4,5,6][1,1,1]] :: Poly Rational Revlex
a49 = p[tp 1 [1,2,3,4][1,1,1,1], tp 1 [2,3,4,5][1,1,1,1], tp 1 [1,2,3,6][1,1,1,1], tp 1 [1,2,5,6][1,1,1,1], tp 1 [1,4,5,6][1,1,1,1], tp 1 [3,4,5,6][1,1,1,1]] :: Poly Rational Revlex
a50 = p[tp 1 [1,2,3,4,5][1,1,1,1,1], tp 1 [1,2,3,4,6][1,1,1,1,1], tp 1 [1,2,3,5,6][1,1,1,1,1], tp 1 [1,2,4,5,6][1,1,1,1,1], tp 1 [1,3,4,5,6][1,1,1,1,1], tp 1 [2,3,4,5,6][1,1,1,1,1]] :: Poly Rational Revlex
a51 = p[t 1 [1,1,1,1,1,1], t (-1) [] ] :: Poly Rational Revlex
ps13 = parps [a46,a47,a48,a49,a50,a51]
----------------------------------------------------------------------------------------------------
a52 = p [t 1 [1,1],tp 1 [2,3][1,1], tp 1 [3,4][1,1], tp 1 [1,5][1,1], tp 1 [4,5][1,1]] :: Poly Rational Revlex
a53 = p [tp 1 [1][1], tp 1 [2][1], tp 1 [3][1], tp 1 [4][1], tp 1 [5][1]] :: Poly Rational Revlex
a54 = p [tp 1 [1,2,3][1,1,1], tp 1 [2,3,4][1,1,1], tp 1 [1,2,5][1,1,1], tp 1 [1,4,5][1,1,1], tp 1 [3,4,5][1,1,1]] :: Poly Rational Revlex
a55 = p [t 1 [1,1,1,1], tp 1 [1,2,4,5][1,1,1,1], tp 1 [1,3,4,5][1,1,1,1], tp 1 [2,3,4,5][1,1,1,1], tp 1 [2,3,4][1,1,1] ] :: Poly Rational Revlex
a56 = p [ t 1 [1,1,1,1,1], t (-1) []] :: Poly Rational Revlex
ps14 = parps [a52, a53,a54,a55,a56]
----------------------------------------------------------------------------------------------------
a57 = p [tp 1 [1,2,3][2,1,1], tp 1 [1,2,3][1,2,1], tp 1 [1,2,3][1,1,2], tp 1 [1,1,1][1,1,1], tp 1 [1,2][1,1], tp 1 [1,3][1,1], tp 1 [2,3][1,1]] :: Poly Rational Revlex
a58 = p [tp 1 [1,2,3][2,2,1], tp 1 [1,2,3][1,2,2], tp 1 [1,2,3][2,1,1], t 1 [1,1,1], tp 1 [2,3][1,1], t 1 [1], tp 1 [3][1] ] :: Poly Rational Revlex
a59 = p [t 1 [2,2,2], t 1 [2,2,1], t 1 [1,2,1], t 1 [1,1,1], tp 1 [1,3][1,1], tp 1 [3][1], t 1 []] :: Poly Rational Revlex
ps15 = parps [a57, a58, a59]
----------------------------------------------------------------------------------------------------


-- class' XE take max
-- totalDeg XE
-- number of poly

--ld XE

-- numTerms

helper :: (NFData t) => PS t Revlex -> [Poly t Revlex]
helper (PS b) = L.map f  (A.toList b )
  where
    f (Idp poly x) = poly

cls ::(NFData t, Eq t, Num t) => PS t Revlex -> [Int]
cls p = L.map class' $ helper ( p)

td :: (NFData t, Eq t, Num t) =>PS t Revlex -> [Int]
td p = L.map totalDeg $ helper ( p)

np :: (NFData t, Eq t, Num t) =>PS t Revlex -> Int
np p = length $ helper (charSetMPS p)
np' :: (NFData t, Eq t, Num t, Fractional t, Ord t) =>PS t Revlex -> Int
np' p = length $ helper (charSetMSP p)
---------------------------------------------------------
lde :: (NFData t, Eq t, Num t) =>PS t Revlex -> [Int]
lde p = L.map (\x-> varDegree x 5) $ helper (charSetMPS p)
lde' :: (NFData t, Eq t, Num t, Fractional t, Ord t) =>PS t Revlex -> [Int]
lde' p = L.map (\x-> varDegree x 3) $ helper (charSetMSP p)

nt :: (NFData t, Eq t, Num t) =>  PS t Revlex -> [Int]
nt p = L.map numTerms $ helper (charSetMPS p)

nt' :: (NFData t, Eq t, Num t, Fractional t, Ord t) =>  PS t Revlex -> [Int]
nt' p = L.map numTerms $ helper (charSetMSP p)


-- cls (mas alto), td, np; class' cambiar el valor; nt
