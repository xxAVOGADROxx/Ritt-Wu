{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.Tasty.QuickCheck as QC
import Test.Tasty
import Lib

reverseTests :: TestTree
reverseTests  = testGroup "Tests over reverse"
  [QC.testProperty "reverse respect lenght" $
   \(lst::[Integer])-> length (reverse' lst) == length lst]




-- import Test.Tasty
-- import Test.Tasty.HUnit as HU
-- import Lib

-- hunitTestInsertOnLeaf :: TestTree
-- hunitTestInsertOnLeaf = HU.testCase "Insert 'a' on empty tree" $
--   assertEqual "Insertion is wrong" (treeInsert 'a' Leaf)(Node 'a' Leaf Leaf)

-- allTests :: TestTree
-- allTests = testGroup "Tasty Test" [
--   testGroup "Hunit Test" [hunitTestInsertOnLeaf]
--  ]

main :: IO()
main = defaultMain reverseTests
