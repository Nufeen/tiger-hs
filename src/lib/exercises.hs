{-# LANGUAGE OverloadedStrings #-}

-- https://www.cs.princeton.edu/~appel/modern/ml/chap1/

module Exercises where

import Test.Hspec

type Key = String

data Tree = Leaf | Node Tree Key Tree
  deriving Show

empty :: Tree
empty = Leaf

insert :: Key -> Tree -> Tree
insert key Leaf = Node Leaf key Leaf
insert key (Node l k r)
  | key < k = Node (insert key l) k r
  | key > k = Node l k (insert key r)
  | otherwise = Node l key r

-- 1.1a

member :: Key -> Tree -> Bool
member _ Leaf = False
member key (Node l k r)
  | key < k = member key l
  | key > k = member key r
  | otherwise  = True

tree :: Tree
tree =
  insert "a" $
  insert "b" $
  insert "c" $
  insert "d" $
  empty

test :: IO ()
test = hspec $ do
  describe "member function" $ do
    it "should find correct ones" $ do
      member "a" tree `shouldBe` True
      member "b" tree `shouldBe` True
      member "c" tree `shouldBe` True
    it "should be falsy on missing ones" $ do
      member "e" tree `shouldBe` False
      member "f" tree `shouldBe` False
      member "" tree `shouldBe` False
