module Main where
import Criterion.Main
import Prelude as P
import Numeric.Algebra as N
import Polynomial.Monomial
--import Polynomial.Prelude


main :: IO ()
main = do
  let x = m [1 .. 10000] :: Monomial Lex 
      y = m [10000 .. 19999] :: Monomial Lex
      z = [1..10000] :: [Double]
      p = [10000 .. 19999] :: [Double]

  defaultMain

    [ bgroup
      "Monomial * - Massive"
        [bench "whnf" $ whnf ((N.*) x) y
        ,bench "nf" $ nf ((N.*) x) y]
    , bgroup
      "Monomial *  - Traditional"
      [bench  "nf" $ nf ((zipWith (P.+)) z) p
      ,bench "whnf" $ whnf ((zipWith (P.+)) z) p ]
    ]


