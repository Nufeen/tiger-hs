{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import Exercises

main :: IO ()
main = hspec $ do
  describe "member function" $ do
    it "should find correct ones" $ do
      member "a" tree `shouldBe` True
      member "b" tree `shouldBe` True
      member "c" tree `shouldBe` True
    it "should be falsy on missing ones" $ do
      member "e" tree `shouldBe` False
      member "f" tree `shouldBe` False
      member "" tree `shouldBe` False
