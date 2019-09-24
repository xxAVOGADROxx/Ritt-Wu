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
      ps = concat $ c1 : c2 : c3 : []
      ps1 =
        [ Poly [Term (1, mp [2] [2]), Term (-1, m [1, 1]), Term (-1, m [0])] :: Poly Rational Revlex
        , Poly [Term (1, m [2]), Term (-2, mp [3] [1])] :: Poly Rational Revlex
        , Poly [Term (1, mp [3] [2]), Term (-1, m [1, 1]), Term (1, m [0])] :: Poly Rational Revlex
        , Poly [Term (3, mp [2] [5])] :: Poly Rational Revlex
        ]
     -- ps1 =
  defaultMain
      -- bgroup "Multiplication" [bgroup "MonomialM" [bench "whnf" $ whnf ((N.*)
      -- x) y ,bench "nf" $ nf force (((N.*) x) y) ,bench "nf-force" $ nf force
      -- (((N.*) x) y) ] ,bgroup "MonomialT" [bench "nf" $ nf ((zipWith (P.+))
      -- z) p --260ns ,bench "nf-force" $ nf force (((zipWith (P.+)) z) p)
      -- --79ns ,bench "whnf" $ whnf ((zipWith (P.+)) z) p ] ]
    [ bgroup
        "Normal division"
        [bench "whnf" $ whnf (psByTf ps) ps1, bench "nf" $ nf (psByTf ps) ps1]
    , bgroup
        "IO division"
        [ bench "whnf" $ whnf (psByTfLRIO ps) ps1
        , bench "nf" $ nf (psByTfLRIO ps) ps1
        ]
    , bgroup
        "[] division"
        [ bench "whnf" $ whnf (psByTfLR ps) ps1
        , bench "nf" $ nf (psByTfLR ps) ps1
        ]
    , bgroup
        "wrapper"
        [bench "whnf" $ whnf (wrapper ps) ps1, bench "nf" $ nf (wrapper ps) ps1]
    ]
