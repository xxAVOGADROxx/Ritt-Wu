module Main where
import Polynomial.Wu
import Polynomial.Polynomial
import Polynomial.Monomial
import Polynomial.Terms

-- http://symbolicdata.org/XMLResources/IntPS/Behnke.xml
a1 = Poly [Term(1,m[7]), Term(-1,mp[2][7])] :: Poly Rational Revlex
a2 = Poly [Term(1,mp[2][7]), Term(-1,mp[3][7])] :: Poly Rational Revlex
a3 = Poly [Term(1,mp[3][7]), Term(-1,mp[4][7])] :: Poly Rational Revlex
a4 = Poly [Term(1,mp[4][7]), Term(-1, mp[5][7])] :: Poly Rational Revlex
a5 = Poly [Term(1,m[6,1]), Term(1, mp[2,3][6,1]),Term(1, mp[3,4][6,1]), Term(1, mp[4,5][6,1]), Term(1, mp[1,5][1,6]) ] :: Poly Rational Revlex
ps1= a1 : a2: a3: a4: a5: []
-- http://symbolicdata.org/XMLResources/IntPS/ZeroDim.example_1.xml
a6 = Poly [Term(1,m[2]), Term(1, mp[2][2]), Term(-10,m[0])] :: Poly Rational Revlex
a7 = Poly [Term(1,m[2]), Term(1, m[1,1]), Term(2,mp[2][2]), Term(-16,m[0])] :: Poly Rational Revlex
ps2 = a6 : a7 :[]




main :: [Poly Rational Revlex]
main = do
  --charSetPfS ps1
  --charSetPfS pall
  charSetPfPr pall
 



-- documentation: cabal haddock
-- bench: cabal new-bench -02, with the compiled file use the bgroup optins
-- execute: cabal new-run
