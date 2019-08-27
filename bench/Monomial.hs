{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Criterion.Main
import Prelude as P hiding ((*))
import Polynomial.Monomial
import Numeric.Algebra -- In this library we define the multiplicative instance

main :: IO ()
main = do
  let x = makeMon [1 .. 20] :: Monomial 'Lex Int
      y = makeMon [20 .. 40] :: Monomial 'Lex Int
  defaultMain
    [ bgroup
      "monMul1"
        [
          bench "whnf" $ whnf ((*) x) y
        , bench "nf" $ whnf ((*) x) y
        ]
    -- , bgroup "monMul2" [bench "massiv whnf" $ whnf (auxiliar x) y ]
    ]
