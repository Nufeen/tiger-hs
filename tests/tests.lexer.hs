{-# LANGUAGE OverloadedStrings #-}

-- https://www.cs.princeton.edu/~appel/modern/ml/chap2/

module Main where

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

main :: IO ()
main = do
  dir <- listDirectory "testcases"
  let files =
        sortOn (\x -> read $ (filter isDigit x) :: Int) $
        filter (\x -> head x /= '.') $ dir
  mapM_ lex files
  where
    tryL = try :: IO a -> IO (Either LexicalError a)
    lex file = do
      fileContent <- readFile $ "testcases" ++ "/" ++ file
      putStrLn $ color Yellow file
      res <-
        (tryL .  putStrLn .  show .  streamToList) $
        runLexer lexer file fileContent
      case res of
        Left e -> putStrLn $ color Red $ "\n" ++ (show e) ++ "\n"
        Right _ -> putStrLn "\n"
