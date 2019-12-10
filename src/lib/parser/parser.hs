{-# LANGUAGE OverloadedStrings #-}

-- https://www.cs.princeton.edu/~appel/modern/ml/chap3/

module Parser where

import System.Environment
import System.Directory
import System.Console.Pretty
import Control.Exception (try)
import Language.Lexer.Applicative
-- import Text.Regex.Applicative (RE)
-- import Text.Regex.Applicative (psym, some, many, anySym, (<|>))
import Data.Char (isSpace, isDigit, isLetter, isAlphaNum)
import Data.List

import Tokens
import Lexer

test :: IO ()
test = do
  dir <- listDirectory "testcases"
  let files =
        take 3 $
        sortOn (\x -> read $ (filter isDigit x) :: Int) $
        filter (\x -> head x /= '.') $ dir
  mapM_ lex files
  where
    lex file = do
      fileContent <- readFile $ "testcases" ++ "/" ++ file
      putStrLn $ color Yellow file
      putStrLn .  show .  streamToList $
        runLexer lexer file fileContent
