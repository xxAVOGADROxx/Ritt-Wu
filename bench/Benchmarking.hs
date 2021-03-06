module Main(main) where
import Criterion.Main
import Prelude as P
import Polynomial.Monomial
import Polynomial.Wu
import Polynomial.Polynomial
import Polynomial.Terms

main :: IO ()
main = do
  let a1 = Poly [Term (1, m [7]), Term (-1, mp [2] [7])] :: Poly Rational Revlex
      a2 =
        Poly [Term (1, mp [2] [7]), Term (-1, mp [3] [7])] :: Poly Rational Revlex
      a3 =
        Poly [Term (1, mp [3] [7]), Term (-1, mp [4] [7])] :: Poly Rational Revlex
      a4 =
        Poly [Term (1, mp [4] [7]), Term (-1, mp [5] [7])] :: Poly Rational Revlex
      a5 =
        Poly
          [ Term (1, m [6, 1])
          , Term (1, mp [2, 3] [6, 1])
          , Term (1, mp [3, 4] [6, 1])
          , Term (1, mp [4, 5] [6, 1])
          , Term (1, mp [1, 5] [1, 6])
          ] :: Poly Rational Revlex
      ps1 = a1 : a2 : a3 : a4 : a5 : []
      ------------------------------------------------
      -- example 6.1 pag 15 (Wang 2004, Epsilon-A14
      a6 =
        Poly
          [ Term (1, mp [2] [1])
          , Term (1, mp [3] [2])
          , Term (1, mp [4] [2])
          , Term (-1, m [2])
          ] :: Poly Rational Revlex
      a7 =
        Poly [Term (1, mp [2, 3] [1, 1]), Term (1, mp [4] [2]), Term (-1, m [])] :: Poly Rational Revlex
      a8 =
        Poly
          [ Term (1, mp [2, 3, 4] [1, 1, 1])
          , Term (-1, mp [2] [2])
          , Term (-1, mp [3] [2])
          , Term (-1, mp [4] [1])
          , Term (1, m [])
          ] :: Poly Rational Revlex
      ps2 = a6 : a7 : a8 : []
      --------------------------------------------------
      -- http://symbolicdata.org/XMLResources/IntPS/DiscrC2.xml
      a9 =
        Poly
          [ Term (1, mp [1, 10] [2, 1])
          , Term (2, mp [1, 2, 11] [1, 1, 1])
          , Term (3, mp [2, 12] [2, 1])
          , Term (1, mp [1, 4] [1, 1])
          , Term (2, mp [2, 5] [1, 1])
          , Term (1, mp [7] [1])
          ] :: Poly Rational Revlex
      a10 =
        Poly
          [ Term (3, mp [1, 9] [2, 1])
          , Term (2, mp [1, 2, 10] [1, 1, 1])
          , Term (1, mp [2, 11] [2, 1])
          , Term (2, mp [1, 3] [1, 1])
          , Term (1, mp [2, 4] [1, 1])
          , Term (1, mp [6] [1])
          ] :: Poly Rational Revlex
      a11 =
        Poly
          [ Term (1, mp [1, 9] [3, 1])
          , Term (1, mp [1, 2, 10] [2, 1, 1])
          , Term (1, mp [1, 2, 11] [1, 2, 1])
          , Term (1, mp [2, 12] [3, 1])
          , Term (1, mp [1, 2, 4] [1, 1, 1])
          , Term (1, mp [2, 5] [2, 1])
          , Term (1, mp [1, 6] [1, 1])
          , Term (1, mp [1, 7] [1, 1])
          , Term (1, mp [8] [1])
          ] :: Poly Rational Revlex
      ps3 = a9 : a10 : a11 : []
      ---------------------------------------------------
      -- http://symbolicdata.org/XMLResources/IntPS/Schiele_1.xml
      a12 =
        Poly
          [ Term (1, mp [1, 2, 3] [2, 4, 1])
          , Term (1, m [2, 4])
          , Term (2, m [2, 2, 1])
          , Term (6, m [2, 2])
          , Term (-4, m [1, 3])
          , Term (1, mp [2, 3] [4, 1])
          , Term (-1, mp [2] [4])
          , Term (1, mp [1, 3] [2, 1])
          , Term (1, m [2])
          , Term (4, m [1, 1])
          , Term (2, mp [2, 3] [2, 1])
          , Term (-6, mp [2] [2])
          , Term (1, mp [3] [1])
          , Term (-1, m [])
          ] :: Poly Rational Revlex
      a13 = Poly [] :: Poly Rational Revlex
      ps4 = a12 : a13 : []
      --------------------------------------------------
      -- http://symbolicdata.org/XMLResources/IntPS/Trinks.xml
      a14 =
        Poly
          [ Term (35, mp [2] [1])
          , Term (40, mp [3] [1])
          , Term (25, mp [4] [1])
          , Term (-27, mp [5] [1])
          ] :: Poly Rational Revlex
      a15 =
        Poly
          [ Term (45, mp [2] [1])
          , Term (35, mp [5] [1])
          , Term (-165, mp [6] [1])
          , Term (-36, m [])
          ] :: Poly Rational Revlex
      a16 =
        Poly
          [Term (-11, mp [5, 6] [1, 1]), Term (3, mp [6] [2]), Term (99, m [1])] :: Poly Rational Revlex
      a17 =
        Poly
          [ Term (25, mp [2, 5] [1, 1])
          , Term (-165, mp [6] [2])
          , Term (15, m [1])
          , Term (30, mp [3] [1])
          , Term (-18, mp [4] [1])
          ] :: Poly Rational Revlex
      a18 =
        Poly
          [ Term (15, mp [2, 4] [1, 1])
          , Term (20, mp [3, 5] [1, 1])
          , Term (-9, m [1])
          ] :: Poly Rational Revlex
      a19 =
        Poly
          [ Term (-11, mp [6] [3])
          , Term (1, m [1, 1])
          , Term (2, mp [3, 4] [1, 1])
          ] :: Poly Rational Revlex
      ps5 = a14 : a15 : a16 : a17 : a18 : a19 : []
      --------------------------------------------------
      -- http://symbolicdata.org/XMLResources/IntPS/ZeroDim.example_14.xml
      a20 =
        Poly
          [ Term (-4, m [1])
          , Term (1, mp [2] [1])
          , Term (1, mp [3] [1])
          , Term (1, mp [4] [1])
          ] :: Poly Rational Revlex
      a21 =
        Poly
          [ Term (-4, m [2])
          , Term (1, mp [2] [2])
          , Term (1, mp [3] [2])
          , Term (1, mp [4] [2])
          , Term (4, m [1])
          , Term (1, mp [2] [1])
          , Term (1, mp [3] [1])
          , Term (1, mp [4] [1])
          , Term (-3, m [])
          ] :: Poly Rational Revlex
      a22 =
        Poly
          [ Term (5, m [3])
          , Term (4, mp [3, 4] [2, 1])
          , Term (3, mp [2] [2])
          , Term (2, mp [1, 4] [1, 1])
          , Term (4, m [1])
          , Term (1, mp [2] [1])
          , Term (1, mp [3] [1])
          , Term (2, mp [4] [1])
          , Term (-1, m [])
          ] :: Poly Rational Revlex
      a23 =
        Poly
          [ Term (5, mp [3] [4])
          , Term (1, mp [4] [3])
          , Term (16, m [2])
          , Term (3, mp [2] [2])
          , Term (-4, mp [4] [1])
          , Term (-1, m [])
          ] :: Poly Rational Revlex
      ps6 = a20 : a21 : a22 : a23 : []
      --------------------------------------------------
      -- http://symbolicdata.org/XMLResources/IntPS/Cyclic_5.xml
      a24 = Poly [Term(1,m[1]), Term(1,mp[2][1]), Term(1, mp[3][1]), Term(1,mp[4][1]), Term(1,mp[5][1])] :: Poly Rational Revlex
      a25 = Poly [Term(1,mp[1,2][1,1]), Term(1,mp[2,3][1,1]),Term(1, mp[3,4][1,1]), Term(1,mp[1,5][1,1]),Term(1,mp[4,5][1,1]) ] :: Poly Rational Revlex
      a26 = Poly [Term(1,mp[1,2,3][1,1,1]), Term(1, mp[2,3,4][1,1,1]), Term(1,mp[1,2,5][1,1,1]),Term(1,mp[1,4,5][1,1,1]),Term(1,mp[3,4,5][1,1,1])] :: Poly Rational Revlex
      a27 = Poly [Term(1,m[1,1,1,1]), Term(1,mp[1,2,3,5][1,1,1,1]), Term(1,mp[1,2,4,5][1,1,1,1]), Term(1,mp[1,3,4,5][1,1,1,1]), Term(1, mp[2,3,4,5][1,1,1,1]) ] :: Poly Rational Revlex
      a28 = Poly [Term(1, m[1,1,1,1,1]), Term(-1,m[])] :: Poly Rational Revlex
      ps7 = a24:a25:a26:a27:a28:[]
      --------------------------------------------------
      -- http://symbolicdata.org/XMLResources/IntPS/Fee_1.xml
      -- q,c,p,d
      a29 = Poly[Term(-2,mp[1,2][1,1]),Term(-2,mp[3][2]), Term(-2, m[1]),Term(8,mp[3][1]), Term(-2,m[])] :: Poly Rational Revlex
      a30 = Poly[Term(-3,m[1,1,1]),Term(2,mp[1,3,4][1,1,1]), Term(4,mp[3,4][2,1]), Term(3,mp[2,3][1,1]), Term(1,mp[1,4][1,1]), Term(-7,mp[3,4][1,1])] :: Poly Rational Revlex
      a31 = Poly[Term(1,m[2,2]), Term(-2,mp[1,2,4][2,1,1]), Term(-2,m[1,1,1,1]), Term(1,mp[1,4][2,2]), Term(2,mp[1,3,4][1,1,2]), Term(1,mp[3,4][2,2]), Term(-2,mp[1,2][1,2]), Term(4,m[1,1,1]), Term(2,mp[1,2,4][1,1,1]), Term(2,mp[2,3,4][1,1,1]), Term(-4,mp[1,4][1,2]), Term(-4,mp[3,4][1,2]), Term(1,mp[2][2]), Term(2,mp[1,3][1,1]), Term(10,mp[3][2]), Term(-4,mp[2,4][1,1]), Term(4,mp[4][2]), Term(-2,m[1]), Term(-8,mp[3][1]), Term(2,m[])] :: Poly Rational Revlex
      a32 = Poly[Term(3,m[2,2]), Term(12, m[1,1,1,1]), Term(-3, mp[1,4][2,2]), Term(6,mp[1,3,4][1,1,2]), Term(-3,mp[3,4][2,2]), Term(-6, mp[1,2][1,2]), Term(12,mp[1,2,4][1,1,1]), Term(12, mp[2,3,4][1,1,1]), Term(-4, m[2]), Term(3,mp[2][2]), Term(5,mp[3][2]), Term(-12,mp[2,4][1,1]), Term(12,mp[4][2]), Term(-6,mp[3][1]), Term(5,m[])] :: Poly Rational Revlex
      ps8 = a29:a30:a31:a32:[]
  defaultMain
  -- [ bgroup
  --     "BsPs"
  --   --      bench "whnf" $ whnf (psByTf ps) ps1
  --     [ bgroup
  --         "prem"
  --         [ bench "PS-nf" $ nf (psBsPS ps) ps1
  --         -- ,bench "S-nf" $ nf (psBsS ps) ps1
  --         ]
  --     , bgroup
  --         "spoly"
  --   --      bench "whnf" $ whnf (psByTfLRIO ps) ps
  --         [ bench "SP-nf" $ nf (psBsSP ps) ps1
  --         --, bench "S-nf" $ nf (psBsSParS ps) ps1
  --         ]
  --     , bgroup
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
--      ]
--    ]
-- ********************************************
   [ bgroup
      "Computing CharSet"
        [ bgroup
            "CharSetPS"
            [--bench "whnf" $ whnf (charSetNormalPS) ps2,
             --bench "nf" $ nf (charSetNormalPS) ps2
            ]
        , bgroup
            "CharSetSP"
            [--bench "whnf" $ whnf (charSetNormalSP) ps2,
             --bench "nf" $ nf (charSetNormalSP) ps2
            ]
         , bgroup
            "CharSetMSeqPS"
            [--bench "whnf" $ whnf (charSetMSeqPS) ps2,
             --bench "nf" $ nf (charSetMSeqPS) ps2
            ]
        , bgroup
            "CharSetMSeqSP"
            [--bench "whnf" $ whnf (charSetMSeqSP) ps2,
             --bench "nf" $ nf (charSetMSeqSP) ps2
            ]
        , bgroup
            "CharSetMSP"
            [--bench "whnf" $ whnf (charSetMSP) ps2,
             --bench "nf" $ nf (charSetMSP) ps2
            ]
        , bgroup
            "CharSetMPS"
            [--bench "whnf" $ whnf (charSetMPS) ps2,
             bench "nf" $ nf (charSetMPS) ps2
            ]
        ]
    ]
