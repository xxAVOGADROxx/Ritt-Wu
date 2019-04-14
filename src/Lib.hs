module Lib
    ( someFunc,
      treeInsert,
      BinaryTree (Node, Leaf)
    ) where

someFunc :: IO ()
someFunc = putStrLn "Jose Seraquive"


data BinaryTree a = Node a (BinaryTree a) (BinaryTree a)
                  |Leaf
                  deriving (Show, Eq)
treeInsert :: ( Ord a) => a -> BinaryTree a -> BinaryTree a
treeInsert x Leaf = Node x Leaf Leaf
treeInsert x (Node y l r) | (x <= y) = Node y (treeInsert x l) r
                          | otherwise = Node y l (treeInsert x r )
