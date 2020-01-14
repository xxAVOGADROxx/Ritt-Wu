module Main where

import Polynomial.Polynomial
import Polynomial.Monomial
import Polynomial.Terms
import Polynomial.Wu
-- -- http://symbolicdata.org/XMLResources/IntPS/Behnke.xml
-- a1 = Poly [Term(1,m[7]), Term(-1,mp[2][7])] :: Poly Rational Revlex
-- a2 = Poly [Term(1,mp[2][7]), Term(-1,mp[3][7])] :: Poly Rational Revlex
-- a3 = Poly [Term(1,mp[3][7]), Term(-1,mp[4][7])] :: Poly Rational Revlex
-- a4 = Poly [Term(1,mp[4][7]), Term(-1, mp[5][7])] :: Poly Rational Revlex
-- a5 = Poly [Term(1,m[6,1]), Term(1, mp[2,3][6,1]),Term(1, mp[3,4][6,1]), Term(1, mp[4,5][6,1]), Term(1, mp[1,5][1,6]) ] :: Poly Rational Revlex
-- ps1= a1 : a2: a3: a4: a5: []
-- -- http://symbolicdata.org/XMLResources/IntPS/ZeroDim.example_1.xml
-- a6 = Poly [Term(1,m[2]), Term(1, mp[2][2]), Term(-10,m[0])] :: Poly Rational Revlex
-- a7 = Poly [Term(1,m[2]), Term(1, m[1,1]), Term(2,mp[2][2]), Term(-16,m[0])] :: Poly Rational Revlex
-- ps2 = a6 : a7 :[]

--a6 = Poly [ Term(1,mp[2][1]), Term(1,mp[3][2]), Term(1,mp[4][2]), Term(-1,m[2]) ] :: Poly Rational Revlex
--a7 = Poly [Term(1,mp[2,3][1,1]), Term(1,mp[4][2]), Term(-1,m[])] :: Poly Rational Revlex
--a8 = Poly [Term(1,mp[2,3,4][1,1,1]), Term(-1,mp[2][2]), Term(-1,mp[3][2]), Term(-1,mp[4][1]), Term(1,m[])] :: Poly Rational Revlex
--ps2 = a6 : a7: a8 : []
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


main :: IO()
main = do --putStrLn "Para usar los benchmar usar cabal new-bench"
   print $ charSetMPS ps1
  --charSetPfS pall
  --charSetPfPr pallR
  --charSetNormalPS ps2
 



-- documentation: cabal haddock
-- bench: cabal new-bench -02, with the compiled file use the bgroup optins
-- execute: cabal new-run
