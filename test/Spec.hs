module Main where

import Test.Tasty
import Test.Tasty.HUnit as HU
import Lib

hunitTestInsertOnLeaf :: TestTree
hunitTestInsertOnLeaf = HU.testCase "Insert 'a' on empty tree" $
  assertEqual "Insertion is wrong" (treeInsert 'a' Leaf)(Node 'a' Leaf Leaf)

allTests :: TestTree
allTests = testGroup "Tasty Test" [
  testGroup "Hunit Test" [hunitTestInsertOnLeaf]
 ]

main :: IO()
main = defaultMain allTests
