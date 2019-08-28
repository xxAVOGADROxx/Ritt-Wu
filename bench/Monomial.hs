-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE DataKinds             #-}
-- {-# LANGUAGE FlexibleContexts      #-}
-- {-# LANGUAGE KindSignatures        #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- module Main where

-- import Criterion.Main
-- import Prelude as P hiding ((*))
-- import Polynomial.Monomial
-- import Numeric.Algebra -- In this library we define the multiplicative instance

-- main :: IO ()
-- main = do
--   let x = makeMon 100 [1 .. 10000] :: Monomial 'Lex Int
--       y = makeMon 100 [10000 .. 20000] :: Monomial 'Lex Int
--       z = [1..10000]
--       p = [10000 .. 20000]

--   defaultMain
--     [ bgroup
--       "monMul1 -  Massive"
--         [bench "whnf" $ whnf ((*) x) y]
--     , bgroup
--       "monMul2 - Traditional"
--       [bench "traditional whnf" $ whnf ((zipWith (P.+)) z) p]
--     ]
--     -- 
