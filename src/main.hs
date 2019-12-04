{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import Lexer

main :: IO ()
main = do
  [file, _] <- getArgs
  fileContent <- readFile file
  print file

-- compile file = do
  -- print $ lexer file
