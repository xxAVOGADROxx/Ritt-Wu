module Main where
import Criterion.Main
import Prelude as P
-- import Numeric.Algebra as N
import Polynomial.Monomial
--import Control.DeepSeq
import Polynomial.Wu
--import Data.List as L
import Polynomial.Polynomial
import Polynomial.Terms


main :: IO ()
main = do
  let c1 =
        [ Poly [Term (1, m [6])]
        , Poly [Term (4, m [3])]
        , Poly [Term (6, m [5])]
        , Poly [Term (10, m [3])]
        ] :: [Poly Rational Revlex]
      c2 =
        [ Poly [Term (1, mp [1, 2] [2, 1]), Term (2, m [3])]
        , Poly [Term (5, mp [2] [5])]
        , Poly [Term (3, mp [1, 2] [1, 1])]
        ] :: [Poly Rational Revlex]
      c3 =
        [ Poly [Term (3, mp [3] [2]), Term (4, mp [1] [2])]
        , Poly [Term (3, mp [3] [1])]
        ] :: [Poly Rational Revlex]
      c4 =
        [ Poly
            [ Term (2, mp [2] [2])
            , Term (-4, mp [2] [1])
            , Term (1, m [2])
            , Term (-4, m [1])
            , Term (3, m [0])
            ]
        ] :: [Poly Rational Revlex]
      c5 =
        [ Poly
            [ Term (1, mp [2] [2])
            , Term (-2, mp [2] [1])
            , Term (3, m [2])
            , Term (-12, m [1])
            , Term (9, m [0])
            ]
        ] :: [Poly Rational Revlex]
      ps = concat $ c1 : c2 : c3 : c4 : c5 : []
      ps1 =
        [ Poly [Term (1, mp [2] [2]), Term (-1, m [1, 1]), Term (-1, m [0])] :: Poly Rational Revlex
        , Poly [Term (1, m [2]), Term (-2, mp [3] [1])] :: Poly Rational Revlex
        , Poly [Term (1, mp [3] [2]), Term (-1, m [1, 1]), Term (1, m [0])] :: Poly Rational Revlex
        , Poly [Term (3, mp [2] [5])] :: Poly Rational Revlex
        , Poly [Term (1, mp [2] [2]), Term (-1, m [1, 1]), Term (-1, m [0])] :: Poly Rational Revlex
        , Poly [Term (1, m [2]), Term (-2, mp [3] [1])] :: Poly Rational Revlex
        , Poly [Term (1, mp [3] [2]), Term (-1, m [1, 1]), Term (1, m [0])] :: Poly Rational Revlex
        , Poly [Term (3, mp [2] [5])] :: Poly Rational Revlex
        , Poly [Term (1, m [2]), Term (-2, mp [3] [1])] :: Poly Rational Revlex
        , Poly [Term (1, mp [3] [2]), Term (-1, m [1, 1]), Term (1, m [0])] :: Poly Rational Revlex
        , Poly [Term (3, mp [2] [5])] :: Poly Rational Revlex
        , Poly [Term (1, mp [2] [2]), Term (-1, m [1, 1]), Term (-1, m [0])] :: Poly Rational Revlex
        , Poly [Term (1, m [2]), Term (-2, mp [3] [1])] :: Poly Rational Revlex
        , Poly [Term (1, mp [3] [2]), Term (-1, m [1, 1]), Term (1, m [0])] :: Poly Rational Revlex
        , Poly [Term (3, mp [2] [5])] :: Poly Rational Revlex
        , Poly [Term (1, m [2]), Term (-2, mp [3] [1])] :: Poly Rational Revlex
        , Poly [Term (1, mp [3] [2]), Term (-1, m [1, 1]), Term (1, m [0])] :: Poly Rational Revlex
        , Poly [Term (3, mp [2] [5])] :: Poly Rational Revlex
        , Poly [Term (1, mp [2] [2]), Term (-1, m [1, 1]), Term (-1, m [0])] :: Poly Rational Revlex
        , Poly [Term (1, m [2]), Term (-2, mp [3] [1])] :: Poly Rational Revlex
        , Poly [Term (1, mp [3] [2]), Term (-1, m [1, 1]), Term (1, m [0])] :: Poly Rational Revlex
        , Poly [Term (3, mp [2] [5])] :: Poly Rational Revlex
        , Poly [Term (1, m [2]), Term (-2, mp [3] [1])] :: Poly Rational Revlex
        , Poly [Term (1, mp [3] [2]), Term (-1, m [1, 1]), Term (1, m [0])] :: Poly Rational Revlex
        , Poly [Term (3, mp [2] [5])] :: Poly Rational Revlex
        , Poly [Term (1, mp [2] [2]), Term (-1, m [1, 1]), Term (-1, m [0])] :: Poly Rational Revlex
        , Poly [Term (1, m [2]), Term (-2, mp [3] [1])] :: Poly Rational Revlex
        , Poly [Term (1, mp [3] [2]), Term (-1, m [1, 1]), Term (1, m [0])] :: Poly Rational Revlex
        , Poly [Term (3, mp [2] [5])] :: Poly Rational Revlex
        , Poly [Term (1, m [2]), Term (-2, mp [3] [1])] :: Poly Rational Revlex
        , Poly [Term (1, mp [3] [2]), Term (-1, m [1, 1]), Term (1, m [0])] :: Poly Rational Revlex
        , Poly [Term (3, mp [2] [5])] :: Poly Rational Revlex
        , Poly [Term (1, mp [2] [2]), Term (-1, m [1, 1]), Term (-1, m [0])] :: Poly Rational Revlex
        , Poly [Term (1, m [2]), Term (-2, mp [3] [1])] :: Poly Rational Revlex
        , Poly [Term (1, mp [3] [2]), Term (-1, m [1, 1]), Term (1, m [0])] :: Poly Rational Revlex
        , Poly [Term (3, mp [2] [5])] :: Poly Rational Revlex
        , Poly [Term (1, m [2]), Term (-2, mp [3] [1])] :: Poly Rational Revlex
        , Poly [Term (1, mp [3] [2]), Term (-1, m [1, 1]), Term (1, m [0])] :: Poly Rational Revlex
        , Poly [Term (3, mp [2] [5])] :: Poly Rational Revlex
        , Poly [Term (1, mp [2] [2]), Term (-1, m [1, 1]), Term (-1, m [0])] :: Poly Rational Revlex
        , Poly [Term (1, m [2]), Term (-2, mp [3] [1])] :: Poly Rational Revlex
        , Poly [Term (1, mp [3] [2]), Term (-1, m [1, 1]), Term (1, m [0])] :: Poly Rational Revlex
        , Poly [Term (3, mp [2] [5])] :: Poly Rational Revlex
        , Poly [Term (1, m [2]), Term (-2, mp [3] [1])] :: Poly Rational Revlex
        , Poly [Term (1, mp [3] [2]), Term (-1, m [1, 1]), Term (1, m [0])] :: Poly Rational Revlex
        , Poly [Term (3, mp [2] [5])] :: Poly Rational Revlex
        , Poly [Term (1, mp [2] [2]), Term (-1, m [1, 1]), Term (-1, m [0])] :: Poly Rational Revlex
        , Poly [Term (1, m [2]), Term (-2, mp [3] [1])] :: Poly Rational Revlex
        , Poly [Term (1, mp [3] [2]), Term (-1, m [1, 1]), Term (1, m [0])] :: Poly Rational Revlex
        , Poly [Term (3, mp [2] [5])] :: Poly Rational Revlex
        , Poly [Term (1, m [2]), Term (-2, mp [3] [1])] :: Poly Rational Revlex
        , Poly [Term (1, mp [3] [2]), Term (-1, m [1, 1]), Term (1, m [0])] :: Poly Rational Revlex
        , Poly [Term (3, mp [2] [5])] :: Poly Rational Revlex
        , Poly [Term (1, mp [2] [2]), Term (-1, m [1, 1]), Term (-1, m [0])] :: Poly Rational Revlex
        , Poly [Term (1, m [2]), Term (-2, mp [3] [1])] :: Poly Rational Revlex
        , Poly [Term (1, mp [3] [2]), Term (-1, m [1, 1]), Term (1, m [0])] :: Poly Rational Revlex
        , Poly [Term (3, mp [2] [5])] :: Poly Rational Revlex
        , Poly [Term (1, m [2]), Term (-2, mp [3] [1])] :: Poly Rational Revlex
        , Poly [Term (1, mp [3] [2]), Term (-1, m [1, 1]), Term (1, m [0])] :: Poly Rational Revlex
        , Poly [Term (3, mp [2] [5])] :: Poly Rational Revlex
        , Poly [Term (1, mp [2] [2]), Term (-1, m [1, 1]), Term (-1, m [0])] :: Poly Rational Revlex
        , Poly [Term (1, m [2]), Term (-2, mp [3] [1])] :: Poly Rational Revlex
        , Poly [Term (1, mp [3] [2]), Term (-1, m [1, 1]), Term (1, m [0])] :: Poly Rational Revlex
        , Poly [Term (3, mp [2] [5])] :: Poly Rational Revlex
        , Poly [Term (1, m [2]), Term (-2, mp [3] [1])] :: Poly Rational Revlex
        , Poly [Term (1, mp [3] [2]), Term (-1, m [1, 1]), Term (1, m [0])] :: Poly Rational Revlex
        , Poly [Term (3, mp [2] [5])] :: Poly Rational Revlex
        , Poly [Term (1, mp [2] [2]), Term (-1, m [1, 1]), Term (-1, m [0])] :: Poly Rational Revlex
        , Poly [Term (1, m [2]), Term (-2, mp [3] [1])] :: Poly Rational Revlex
        , Poly [Term (1, mp [3] [2]), Term (-1, m [1, 1]), Term (1, m [0])] :: Poly Rational Revlex
        , Poly [Term (3, mp [2] [5])] :: Poly Rational Revlex
        , Poly [Term (1, m [2]), Term (-2, mp [3] [1])] :: Poly Rational Revlex
        , Poly [Term (1, mp [3] [2]), Term (-1, m [1, 1]), Term (1, m [0])] :: Poly Rational Revlex
        , Poly [Term (3, mp [2] [5])] :: Poly Rational Revlex
        , Poly [Term (1, mp [2] [2]), Term (-1, m [1, 1]), Term (-1, m [0])] :: Poly Rational Revlex
        , Poly [Term (1, m [2]), Term (-2, mp [3] [1])] :: Poly Rational Revlex
        , Poly [Term (1, mp [3] [2]), Term (-1, m [1, 1]), Term (1, m [0])] :: Poly Rational Revlex
        , Poly [Term (3, mp [2] [5])] :: Poly Rational Revlex
        , Poly [Term (1, m [2]), Term (-2, mp [3] [1])] :: Poly Rational Revlex
        , Poly [Term (1, mp [3] [2]), Term (-1, m [1, 1]), Term (1, m [0])] :: Poly Rational Revlex
        , Poly [Term (3, mp [2] [5])] :: Poly Rational Revlex
        , Poly [Term (1, mp [2] [2]), Term (-1, m [1, 1]), Term (-1, m [0])] :: Poly Rational Revlex
        , Poly [Term (1, m [2]), Term (-2, mp [3] [1])] :: Poly Rational Revlex
        , Poly [Term (1, mp [3] [2]), Term (-1, m [1, 1]), Term (1, m [0])] :: Poly Rational Revlex
        , Poly [Term (3, mp [2] [5])] :: Poly Rational Revlex
        , Poly [Term (1, m [2]), Term (-2, mp [3] [1])] :: Poly Rational Revlex
        , Poly [Term (1, mp [3] [2]), Term (-1, m [1, 1]), Term (1, m [0])] :: Poly Rational Revlex
        , Poly [Term (3, mp [2] [5])] :: Poly Rational Revlex
        , Poly [Term (1, mp [2] [2]), Term (-1, m [1, 1]), Term (-1, m [0])] :: Poly Rational Revlex
        , Poly [Term (1, m [2]), Term (-2, mp [3] [1])] :: Poly Rational Revlex
        , Poly [Term (1, mp [3] [2]), Term (-1, m [1, 1]), Term (1, m [0])] :: Poly Rational Revlex
        , Poly [Term (3, mp [2] [5])] :: Poly Rational Revlex
        , Poly [Term (1, m [2]), Term (-2, mp [3] [1])] :: Poly Rational Revlex
        , Poly [Term (1, mp [3] [2]), Term (-1, m [1, 1]), Term (1, m [0])] :: Poly Rational Revlex
        , Poly [Term (3, mp [2] [5])] :: Poly Rational Revlex
        , Poly [Term (1, mp [2] [2]), Term (-1, m [1, 1]), Term (-1, m [0])] :: Poly Rational Revlex
        , Poly [Term (1, m [2]), Term (-2, mp [3] [1])] :: Poly Rational Revlex
        , Poly [Term (1, mp [3] [2]), Term (-1, m [1, 1]), Term (1, m [0])] :: Poly Rational Revlex
        , Poly [Term (3, mp [2] [5])] :: Poly Rational Revlex
        , Poly [Term (1, m [2]), Term (-2, mp [3] [1])] :: Poly Rational Revlex
        , Poly [Term (1, mp [3] [2]), Term (-1, m [1, 1]), Term (1, m [0])] :: Poly Rational Revlex
        , Poly [Term (3, mp [2] [5])] :: Poly Rational Revlex
        , Poly [Term (1, mp [2] [2]), Term (-1, m [1, 1]), Term (-1, m [0])] :: Poly Rational Revlex
        , Poly [Term (1, m [2]), Term (-2, mp [3] [1])] :: Poly Rational Revlex
        , Poly [Term (1, mp [3] [2]), Term (-1, m [1, 1]), Term (1, m [0])] :: Poly Rational Revlex
        , Poly [Term (3, mp [2] [5])] :: Poly Rational Revlex
        , Poly [Term (1, m [2]), Term (-2, mp [3] [1])] :: Poly Rational Revlex
        , Poly [Term (1, mp [3] [2]), Term (-1, m [1, 1]), Term (1, m [0])] :: Poly Rational Revlex
        , Poly [Term (3, mp [2] [5])] :: Poly Rational Revlex
        , Poly [Term (1, mp [2] [2]), Term (-1, m [1, 1]), Term (-1, m [0])] :: Poly Rational Revlex
        , Poly [Term (1, m [2]), Term (-2, mp [3] [1])] :: Poly Rational Revlex
        , Poly [Term (1, mp [3] [2]), Term (-1, m [1, 1]), Term (1, m [0])] :: Poly Rational Revlex
        , Poly [Term (3, mp [2] [5])] :: Poly Rational Revlex
        , Poly [Term (1, m [2]), Term (-2, mp [3] [1])] :: Poly Rational Revlex
        , Poly [Term (1, mp [3] [2]), Term (-1, m [1, 1]), Term (1, m [0])] :: Poly Rational Revlex
        , Poly [Term (3, mp [2] [5])] :: Poly Rational Revlex
        , Poly [Term (1, mp [2] [2]), Term (-1, m [1, 1]), Term (-1, m [0])] :: Poly Rational Revlex
        , Poly [Term (1, m [2]), Term (-2, mp [3] [1])] :: Poly Rational Revlex
        , Poly [Term (1, mp [3] [2]), Term (-1, m [1, 1]), Term (1, m [0])] :: Poly Rational Revlex
        , Poly [Term (3, mp [2] [5])] :: Poly Rational Revlex
        , Poly [Term (1, m [2]), Term (-2, mp [3] [1])] :: Poly Rational Revlex
        , Poly [Term (1, mp [3] [2]), Term (-1, m [1, 1]), Term (1, m [0])] :: Poly Rational Revlex
        , Poly [Term (3, mp [2] [5])] :: Poly Rational Revlex
        , Poly [Term (1, mp [2] [2]), Term (-1, m [1, 1]), Term (-1, m [0])] :: Poly Rational Revlex
        , Poly [Term (1, m [2]), Term (-2, mp [3] [1])] :: Poly Rational Revlex
        , Poly [Term (1, mp [3] [2]), Term (-1, m [1, 1]), Term (1, m [0])] :: Poly Rational Revlex
        , Poly [Term (3, mp [2] [5])] :: Poly Rational Revlex
        ]
      f1 =
        Poly [Term (1, mp [2] [2]), Term (-1, m [1, 1]), Term (-1, m [0])] :: Poly Rational Revlex
      f2 = Poly [Term (1, m [2]), Term (-2, mp [3] [1])] :: Poly Rational Revlex
      f3 =
        Poly [Term (1, mp [3] [2]), Term (-1, m [1, 1]), Term (1, m [0])] :: Poly Rational Revlex
      pall = [f1,f2,f3] :: [Poly Rational Revlex]
  defaultMain
      -- bgroup "Multiplication" [bgroup "MonomialM" [bench "whnf" $ whnf ((N.*)
      -- x) y ,bench "nf" $ nf force (((N.*) x) y) ,bench "nf-force" $ nf force
      -- (((N.*) x) y) ] ,bgroup "MonomialT" [bench "nf" $ nf ((zipWith (P.+))
      -- z) p --260ns ,bench "nf-force" $ nf force (((zipWith (P.+)) z) p)
      -- --79ns ,bench "whnf" $ whnf ((zipWith (P.+)) z) p ] ]
    [  bgroup
        "Normal-division"
        [
    --      bench "whnf" $ whnf (psByTf ps) ps1
         bench "nf" $ nf (psByTf ps) ps1]
    , bgroup
        "IO-division"
        [
    --      bench "whnf" $ whnf (psByTfLRIO ps) ps1
         bench "nf" $ nf (psByTfLRIO ps) ps1
        ]
    , bgroup
        "Pseudo-Division"
        [
    --      bench "whnf" $ whnf (psByTfLRt ps) ps1
         bench "nf" $ nf (psByTfLRt ps) ps1
         ]
    , bgroup
      "Euclidean-Division"
        [
        bench "nf" $ nf (psByTfLRtS ps) ps1
        ]
    -- , bgroup
    --     "computing charset"
    --     [ bgroup
    --         "Normal charSet"
    --         [--bench "whnf" $ whnf (charSet) pall
    --          bench "nf" $ nf (charSet) pall]
    --     , bgroup
    --         "Pseudo charSet"
    --         [ --bench "whnf" $ whnf (charSetPfPr) pall
    --          bench "nf" $ nf (charSetPfPr) pall
    --         ]
    --     , bgroup
    --         "sPoly charSet"
    --         [ --bench "whnf" $ whnf (charSetPfS) pall
    --          bench "nf" $ nf (charSetPfS) pall
    --         ]
    --     ]
    ]
