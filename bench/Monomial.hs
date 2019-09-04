module Main where
import Criterion.Main
import Prelude as P
import Numeric.Algebra as N
import Polynomial.Monomial
import Control.DeepSeq
--import Polynomial.Prelude


main :: IO ()
main = do
  let x = m [1 .. 10000] :: Monomial Lex 
      y = m [10000 .. 19999] :: Monomial Lex
      z = [1..10000] :: [Double]
      p = [10000 .. 19999] :: [Double]

  defaultMain

    [ bgroup
      "Multiplication"
      [bgroup
        "MonomialM"
        [bench "whnf" $ whnf ((N.*) x) y
        ,bench "nf" $ nf force (((N.*) x) y)
        ,bench "nf-force" $ nf force (((N.*) x) y)
        ]
      ,bgroup
        "MonomialT"
        [bench  "nf" $ nf ((zipWith (P.+)) z) p --260ns
        ,bench  "nf-force" $ nf force (((zipWith (P.+)) z) p) --79ns
        ,bench "whnf" $ whnf ((zipWith (P.+)) z) p ]
      ]
    ]


