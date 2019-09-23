module Main where
--import Prelude as P
import Polynomial.Wu
import Criterion.Main
--import Data.Massiv.Array as A
import Polynomial.Monomial
import Control.Scheduler

main :: IO()
main = putStrLn "jose"

oksana :: (Num t) =>  [t] -> [(t, t)]
oksana xs = zip (map ((+)1) [1,2,3]) xs


-- documentation: cabal haddock
-- bench: cabal new-bench -02, with the compiled file use the bgroup optins
-- execute: cabal new-run
