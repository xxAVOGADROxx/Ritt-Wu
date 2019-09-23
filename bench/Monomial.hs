module Main where
import Criterion.Main
import Prelude as P
import Numeric.Algebra as N
import Polynomial.Monomial
import Control.DeepSeq
import Polynomial.Wu
import Data.List as L
import Polynomial.Polynomial
import Polynomial.Terms


main :: IO ()
main = do
  let a = 1000
      b = 1000000
      c1 = [Poly[Term(1,m[6])], Poly[Term(4,m[3])], Poly[Term(6,m[5])], Poly[Term(10,m[3])]] :: [Poly Rational Revlex]
      c2 = [Poly[Term(1, mp[1,2][2,1]), Term(2, m[3])], Poly[Term(5,mp[2][5])], Poly[Term(3,mp[1,2][1,1])]] :: [Poly Rational Revlex]
      c3 = [Poly[Term(3, mp[3][2]), Term(4, mp[1][2])], Poly[Term(3, mp[3][1])]] :: [Poly Rational Revlex]
      ps = concat $ c1:c2:c3:[]
     -- ps1 = 
  defaultMain

    [
      -- bgroup
      -- "Multiplication"
      -- [bgroup
      --   "MonomialM"
      --   [bench "whnf" $ whnf ((N.*) x) y
      --   ,bench "nf" $ nf force (((N.*) x) y)
      --   ,bench "nf-force" $ nf force (((N.*) x) y)
      --   ]
      -- ,bgroup
      --   "MonomialT"
      --   [bench  "nf" $ nf ((zipWith (P.+)) z) p --260ns
      --   ,bench  "nf-force" $ nf force (((zipWith (P.+)) z) p) --79ns
      --   ,bench "whnf" $ whnf ((zipWith (P.+)) z) p ]
      -- ]
      bgroup
      "Polynomial Division"
      [bgroup
       "laia"
       [bench "whnf" $ whnf (laia a) b
       ,bench "whnf" $ whnf (laia' a) b
       ,bench "whnf" $ whnf (laia'') b
       ,bench "whnf repliW" $ whnf (laiaReplicateWork a) b
       --,bench "nf" $ nf (laiaReplicateWork a ) b 
       ,bench "whnf" $ whnf (\x -> L.foldl' (P.+) 0 [0 .. x]) b
       ,bench "nf" $ nf (\x -> L.foldl' (P.+) 0 [0 .. x]) b
       ]
      ]
    ]


