module Main where
import Prelude as P
-- import Polynomial.Monomial
import Criterion.Main
import Data.Massiv.Array as A
import Polynomial.Monomial

x = m [1 .. 20] :: Monomial Lex Int
y = m [20 .. 40] :: Monomial Lex Int

main :: IO ()
main = putStrLn "Jose"

integrate :: Int -> (Double -> Double) -> Double -> Double -> Double
integrate n f a b =
  let step = (b - a) / fromIntegral n
      segments = fmap (\x -> a + fromIntegral x * step) (range Par 0 n) 
      area x = step * (f x + f(x + step)) /2
  in  P.sum $ fmap area segments 
-- documentation: cabal haddock
-- bench: cabal new-bench -02, with the compiled file use the bgroup optins
-- execute: cabal new-run
