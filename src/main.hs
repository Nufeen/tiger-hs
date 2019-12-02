{-# LANGUAGE OverloadedStrings #-}

module Main where

import Interprete

main :: IO ()
main = putStrLn . show  $ maxArgs program
