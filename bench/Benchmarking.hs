module Main(main) where
import Criterion.Main
import Prelude as P
import Polynomial.Monomial
import Polynomial.Wu
import Polynomial.Polynomial
--import Polynomial.Terms
main :: IO (PS Rational Revlex)
main
      --Example 1 book calculator
 = do
  let f1 = p [tp 1 [2] [2], t (-1) [1, 1], t (-1) []] :: Poly Rational Revlex
      f2 = p [t 1 [2], tp (-2) [3] [1]] :: Poly Rational Revlex
      f3 = p [tp 1 [3] [2], t (-1) [1, 1], t 1 []] :: Poly Rational Revlex
      ps1 = parps [f2,f1, f3]
      -- --------------------------------------------------
      -- -- example 6.1 pag 15 (Wang 2004, Epsilon-A14)
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
      -- http://symbolicdata.org/XMLResources/IntPS/DiscrC2.xml
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
      ---------------------------------------------------
      -- http://symbolicdata.org/XMLResources/IntPS/Schiele_1.xml

      a12 = p [ tp 1 [1,2,3][2,4,1], tp 1 [1,2][2,4], tp 2 [1,2,3][2,2,1], tp 6 [1,2][2,2], tp (-4) [1,2][1,3], tp 1 [2,3][4,1], tp (-1) [2][4], tp 1 [1,3][2,1], tp 1 [1][2], tp 4 [1,2][1,1], tp 2 [2,3][2,1], tp (-6) [2][2], tp 1 [3][1], t (-1) []
              ] :: Poly Rational Revlex
      a13 = p [ tp 1 [1,2,3][8,3,1], tp 1 [1,2,3][8,1,1], tp 4 [1,2,3][6,3,1], tp (-8) [1,2][6,3], tp 8 [1,2][5,4], tp 4 [1,2,3][6,1,1], tp 8 [1,2][6,1], tp (-48) [1,2][5,2], tp 6 [1,2,3][4,3,1], tp 48 [1,2][4,3], tp (-8) [1,2][3,4], tp 8 [1][5], tp 6 [1,2,3][4,1,1], tp (-48) [1,2][4,1], tp (48) [1,2][3,2], tp 4 [1,2,3][2,3,1], tp (-8) [1,2][2,3], tp (-8) [1][3], tp 4 [1,2,3][2,1,1], tp (8) [1,2][2,1], tp (1) [2,3][3,1], tp 1 [2,3][1,1]
               ] :: Poly Rational Revlex
      ps4 = parps [a12,a13]
      --------------------------------------------------
      -- http://symbolicdata.org/XMLResources/IntPS/Trinks.xml
      a14 = p[ tp 35 [2][1], tp 40 [3][1], tp 25 [4][1], tp (-27) [5][1]] :: Poly Rational Revlex
      a15 = p[ tp 45 [2][1], tp 35 [5][1], tp (-165) [6][1], t (-36) []] :: Poly Rational Revlex
      a16 = p[ tp (-11)[5,6][1,1], tp (3)[6][2],tp (99) [1][1]  ] :: Poly Rational Revlex
      a17 = p[ tp 25 [2,5][1,1], tp (-165)[6][2], tp (15) [1][1], tp 30 [3][1], tp (-18) [4][1] ] :: Poly Rational Revlex
      a18 = p[ tp 15 [2,4][1,1], tp 20 [3,5][1,1], tp (-9) [1][1]] :: Poly Rational Revlex
      a19 = p[ tp (-11)[6][3], tp 1 [1,2][1,1], tp 2 [3,4][1,1] ]  :: Poly Rational Revlex
      ps5 = parps [a14,a15,a16,a17,a18,a19]
      -- --------------------------------------------------
      -- http://symbolicdata.org/XMLResources/IntPS/ZeroDim.example_14.xml
      a20 = p[t  (-4) [1], tp 1 [2][1], tp 1 [3][1], tp 1 [4][1]] :: Poly Rational Revlex
      a21 = p[tp (-4) [1][2], tp 1 [2][2], tp 1 [3][2], tp 1 [4][2], tp 4 [1][1], tp 1 [2][1], tp 1 [3][1], tp 1 [4][1], t (-3) []] :: Poly Rational Revlex
      a22 = p[tp (5) [1][3], tp 4 [3,4][2,1], tp 3 [2][2], tp 2 [1,4][1,1], tp 4 [1][1], tp 1 [2][1], tp 1 [3][1], tp 2 [4][1], t (-1) []] :: Poly Rational Revlex
      a23 = p[tp 5 [3][4], tp 1 [4][3], tp 16 [1][2], tp 3 [2][2], tp (-4) [4][1], t (-1) []] :: Poly Rational Revlex
      ps6 = parps [a20,a21,a22,a23]
      --------------------------------------------------
      -- http://symbolicdata.org/XMLResources/IntPS/Cyclic_5.xml
      -- v,w,x,y,z
      -- 1,2,3,4,5
      a24 = p[tp 1 [1][1], tp 1 [2][1], tp 1 [3][1], tp 1 [4][1], tp 1 [5][1]] :: Poly Rational Revlex
      a25 = p[tp 1 [1,2][1,1], tp 1 [2,3][1,1], tp 1 [3,4][1,1], tp 1 [1,5][1,1], tp 1 [4,5][1,1]] :: Poly Rational Revlex
      a26 = p[tp 1 [1,2,3][1,1,1], tp 1 [2,3,4][1,1,1], tp 1 [1,2,5][1,1,1], tp 1 [1,4,5][1,1,1], tp 1 [3,4,5][1,1,1]] :: Poly Rational Revlex
      a27 = p[tp 1 [1,2,3,4][1,1,1,1], tp 1 [1,2,3,5][1,1,1,1], tp 1 [1,2,4,5][1,1,1,1], tp 1 [1,3,4,5][1,1,1,1], tp 1 [2,3,4,5][1,1,1,1]] :: Poly Rational Revlex
      a28 = p[t 1 [1,1,1,1,1], t (-1) []] :: Poly Rational Revlex
      ps7 = parps [a24,a25,a26, a27, a28]
      --------------------------------------------------
      -- http://symbolicdata.org/XMLResources/IntPS/Fee_1.xml q,c,p,d a29 =
      -- q,c,p,d
      -- 1,2,3,4
      a29 = p[tp (-2) [1,3][1,1], tp (-2) [3][2], tp (-2)[1][1], tp (8) [3][1], t (-2) []]:: Poly Rational Revlex
      a30 = p[tp (-3)[1,2,3][1,1,1], tp (2) [1,3,4][1,1,1], tp (4)[3,4][2,1], tp (3)[2,3][1,1], tp 1 [1,4][1,1], tp (-7) [3,4][1,1]]:: Poly Rational Revlex
      a31 = p[tp 1 [1,2][2,2], tp (-2)[1,2,4][2,1,1], t (-2) [1,1,1,1], tp 1 [1,4][2,2], tp 2 [1,3,4][1,1,2], tp 1 [3,4][2,2], tp (-2)[1,2][1,2], t (4) [1,1,1], tp 2 [1,2,4][1,1,1], tp 2 [2,3,4][1,1,1], tp (-4)[1,4][1,2], tp (-4) [3,4][1,2], tp 1 [2][2], tp 2 [1,3][1,1], tp 10 [3][2], tp (-4)[2,4][1,1], tp 4 [4][2], tp (-2)[1][1], tp (-8)[3][1], t 2 [] ]:: Poly Rational Revlex
      a32 = p[t 3 [2,2], t 12 [1,1,1,1], tp (-3) [1,4][2,2], tp 6 [1,3,4][1,1,2], tp (-3) [3,4][2,2], tp (-6) [1,2][1,2], tp 12 [1,2,4][1,1,1], tp 12 [2,3,4][1,1,1], tp (-4) [1][2], tp 3 [2][2], tp 5 [3][2], tp (-12) [2,4][1,1], tp 12 [4][2], tp (-6) [3][1], t 5 [] ] :: Poly Rational Revlex
      ps8 = parps [a29,a30,a31,a32]
      --------------------------------------------------
      -- http://symbolicdata.org/XMLResources/IntPS/Weispfenning-94.xml
      a33 = p[tp 1 [2][4], tp 1 [1,2,3][1,2,1], t 1 [2], t (-2)[1,1], tp 1 [2][1], tp 1 [3][2] ] :: Poly Rational Revlex
      a34 = p[tp 1 [1,2][1,4], tp 1 [2,3][1,4], t (-2) [2,1], t (-3) []]:: Poly Rational Revlex
      a35 = p[t (-1) [3,2], t 1 [1,1,3],  tp 1 [2][4], t 1 [1,2,1], t (-2)[1,1]]:: Poly Rational Revlex
      ps9 = parps [a33,a34,a35]
      --------------------------------------------------
      -- http://symbolicdata.org/XMLResources/IntPS/Fateman.xml
      a36 = p[t 2 [3], tp 2 [2][3], tp 2 [3][3], tp 1 [4][3]]:: Poly Rational Revlex
      -- 6*p^4*q+4*p^3*q^2+2*p^2*q^3+4*p*q^4+6*p^4*r+8*p^3*q*r+4*p*q^3*r+6*q^4*r+4*p^3*r^2+4*q^3*r^2+2*p^2*r^3+4*p*q*r^3+2*q^2*r^3+4*p*r^4+4*q*r^4+3*p^4*s+4*p^3*q*s+2*p*q^3*s+3*q^4*s+4*p^3*r*s+4*q^3*r*s+2*p*r^3*s+2*q*r^3*s+3*r^4*s+p^3*s^2+q^3*s^2+r^3*s^2+p^2*s^3+2*p*q*s^3+q^2*s^3+2*p*r*s^3+2*q*r*s^3+r^2*s^3+p*s^4+q*s^4+r*s^4-s^5
      a37 = p[]:: Poly Rational Revlex
      a38 = p[t 2 [5], tp 2 [2][5], tp 2 [3][5], tp 1 [4][5]]:: Poly Rational Revlex
      ps10 = parps [a36,a37,a38]
      -- http://symbolicdata.org/XMLResources/IntPS/Sym3_5.xml
      a39 = p[tp 1 [2,3][1,5], t 1 [1], t (-2)[] ]:: Poly Rational Revlex
      a40 = p[tp 1 [1,3][5,1], tp 1 [2][1], t (-2)[]]:: Poly Rational Revlex
      a41 = p[t 1 [1,5], tp 1 [3][1], t (-2) []]:: Poly Rational Revlex
      ps11 = parps [a39,a40,a41]
      -- http://symbolicdata.org/XMLResources/IntPS/Wu-90.xml -- no seguro 4
      -- http://symbolicdata.org/XMLResources/IntPS/Cyclic_6.xml
      a46 = p[tp 1 [1][1], tp 1 [2][1], tp 1 [3][1], tp 1 [4][1], tp 1 [5][1], tp 1 [6][1]] :: Poly Rational Revlex
      a47 = p[tp 1 [1,2][1,1], tp 1 [2,3][1,1], tp 1 [3,4][1,1], tp 1 [4,5][1,1], tp 1 [1,6][1,1], tp 1 [5,6][1,1]] :: Poly Rational Revlex
      a48 = p[tp 1 [1,2,3][1,1,1], tp 1 [2,3,4][1,1,1], tp 1[3,4,5][1,1,1], tp 1 [1,2,6][1,1,1], tp 1 [1,5,6][1,1,1], tp 1 [4,5,6][1,1,1]] :: Poly Rational Revlex
      a49 = p[tp 1 [1,2,3,4][1,1,1,1], tp 1 [2,3,4,5][1,1,1,1], tp 1 [1,2,3,6][1,1,1,1], tp 1 [1,2,5,6][1,1,1,1], tp 1 [1,4,5,6][1,1,1,1], tp 1 [3,4,5,6][1,1,1,1]] :: Poly Rational Revlex
      a50 = p[tp 1 [1,2,3,4,5][1,1,1,1,1], tp 1 [1,2,3,4,6][1,1,1,1,1], tp 1 [1,2,3,5,6][1,1,1,1,1], tp 1 [1,2,4,5,6][1,1,1,1,1], tp 1 [1,3,4,5,6][1,1,1,1,1], tp 1 [2,3,4,5,6][1,1,1,1,1]] :: Poly Rational Revlex
      a51 = p[t 1 [1,1,1,1,1,1], t (-1) [] ] :: Poly Rational Revlex
      ps13 = parps [a46,a47,a48,a49,a50,a51]
  --defaultMain
   -- [bgroup
   --    "Polynomial Division"
   --  --      bench "whnf" $ whnf (psByTf ps) ps1
   --    [ bgroup
   --        "prem"
   --        [ bench "PS-nf" $ nf (psBsUParP ps2) ps1
   --        -- ,bench "S-nf" $ nf (psBsS ps) ps1
   --        ]
   --    , bgroup
   --        "spoly"
    --      bench "whnf" $ whnf (psByTfLRIO ps) ps
    --      [ bench "SP-nf" $ nf (psBsUParS ps2) ps1
          --, bench "S-nf" $ nf (psBsSParS ps) ps1
    --      ]
  --    , bgroup
  --         "Sequential pre"
  --         [ bench "Seq PS-nf" $ nf (psBsSeqPS ps) ps1
  --          --, bench "S-nf" $ nf (psBsSSeqS ps) ps1
  --         ]
  --     , bgroup
  --         "Sequential spoly"
  --         [ bench "Seq SP-nf" $ nf (psBsSeqSP ps) ps1
  --    -- ,bench "P-nf" $ nf (ps)
  --         ]
  --     , bgroup
  --         "Parallel prem"
  --   --      bench "whnf" $ whnf (psByTfLRt ps) ps1
  --         [bench "Par PS-nf" $ nf (psBsUParP ps) ps1]
  --     , bgroup
  --       "Parallel spoly"
  --         [bench "Par SP-nf" $ nf (psBsUParS)
  --         ]
  -- ]
  --    ]
   -- ********************************************
   -- [ bgroup
   --    "Computing CharSet"
   --      [
   --        bgroup
   --          "CharSetPS"
   --          [--bench "whnf" $ whnf (charSetNormalPS) ps2,
   --           --bench "nf" $ nf (charSetNormalPS) ps2
   --          ]
   --      , bgroup
   --          "CharSetSP"
   --          [--bench "whnf" $ whnf (charSetNormalSP) ps2,
   --           --bench "nf" $ nf (charSetNormalSP) ps2
   --          ]
   --       , bgroup
   --          "CharSetMSeqPS"
   --          [--bench "whnf" $ whnf (charSetMSeqPS) ps2,
   --           --bench "nf" $ nf (charSetMSeqPS) ps2
   --          ]
   --      , bgroup
   --          "CharSetMSeqSP"
   --          [--bench "whnf" $ whnf (charSetMSeqSP) ps2,
   --           --bench "nf" $ nf (charSetMSeqSP) ps2
   --          ]
   --      , bgroup
   --          "CharSetMSP"
   --          [--bench "whnf" $ whnf (charSetMSP) pall,
   --            bench "nf" $ nf (charSetMSP) pall
   --          ]
   --      , bgroup
   --          "CharSetMPS"
   --          [--bench "whnf" $ whnf (charSetMPS) pall,
   --           --bench "nf" $ nf (charSetMPS) ps2
   --          ]
   --      ]
   --  ]
-- ./Ritt-Wu-benchmark +RTS -N -l
-- threadscope Ritt-Wu-benchmark.eventlog

-- el comando anterior genera estos achivos por lo cual es innecesario la siguiente linea
-- ./Ritt-Wu-benchmark +RTS  -p -RTS
-- cat Ritt-Wu-benchmark.prof > profiling.txt

-- hp2ps -c  Ritt-Wu-benchmark.hp
-- gnome-open Ritt-Wu-benchmark.ps
  --return $ charSetMPS ps6 -- excelent 
  --return $ charSetMSP ps5  -- excelent to parallel
  --return $ charSetMPS ps7
  --return $ charSetMSP ps8 --- parallel
  --return $ charSetMPS ps9 -- parallel
  return $ charSetMPS ps13
