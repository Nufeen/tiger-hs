{-# LANGUAGE OverloadedStrings #-}

-- https://www.cs.princeton.edu/~appel/modern/ml/chap2/

module Lexer (lexer) where

import System.Environment
import System.Directory
import System.Console.Pretty
import Control.Exception (try)
import Language.Lexer.Applicative
import Text.Regex.Applicative (RE)
import Text.Regex.Applicative (psym, some, many, anySym, (<|>))
import Data.Char (isSpace, isDigit, isLetter, isAlphaNum)
import Data.List
import Tokens

lexer :: Lexer Token
lexer = mconcat
  [ token      (longest lexeme)
  , whitespace (longest $ psym isSpace)
  , whitespace (longestShortest commentPrefix commentSuffix)
  ]

lexeme :: RE Char Token
lexeme =
      TokenWHILE      <$ "while"
  <|> TokenFOR        <$ "for"
  <|> TokenTO         <$ "to"
  <|> TokenBREAK      <$ "break"
  <|> TokenLET        <$ "let"
  <|> TokenIN         <$ "in"
  <|> TokenEND        <$ "end"
  <|> TokenFUNCTION   <$ "function"
  <|> TokenVAR        <$ "var"
  <|> TokenTYPE       <$ "type"
  <|> TokenARRAY      <$ "array"
  <|> TokenIF         <$ "if"
  <|> TokenTHEN       <$ "then"
  <|> TokenELSE       <$ "else"
  <|> TokenDO         <$ "do"
  <|> TokenOF         <$ "of"
  <|> TokenNIL        <$ "nil"
  <|> TokenCOMMA      <$ ","
  <|> TokenCOLON      <$ ":"
  <|> TokenSEMICOLON  <$ ";"
  <|> TokenLPAREN     <$ "("
  <|> TokenRPAREN     <$ ")"
  <|> TokenLBRACK     <$ "["
  <|> TokenRBRACK     <$ "]"
  <|> TokenLBRACE     <$ "{"
  <|> TokenRBRACE     <$ "}"
  <|> TokenDOT        <$ "."
  <|> TokenPLUS       <$ "+"
  <|> TokenMINUS      <$ "-"
  <|> TokenTIMES      <$ "*"
  <|> TokenDIVIDE     <$ "/"
  <|> TokenEQ         <$ "="
  <|> TokenNEQ        <$ "<>"
  <|> TokenLT         <$ "<"
  <|> TokenLE         <$ "<="
  <|> TokenGT         <$ ">"
  <|> TokenGE         <$ ">="
  <|> TokenAND        <$ "&"
  <|> TokenOR         <$ "|"
  <|> TokenASSIGN     <$ ":="
  <|> TokenID         <$> tigerID
  <|> TokenINT        <$> tigerINT
  <|> TokenSTRING     <$> tigerSTR

tigerSTR :: RE Char String
tigerSTR = concat <$> sequenceA [ q, str, q ]
  where
    str = many anySym
    q = "\""

tigerINT :: RE Char Int
tigerINT = fmap read $ some $ psym isDigit

tigerID :: RE Char String
tigerID = (:) <$> psym isValidChar <*> many (psym isAlphaNum)
  where
    isValidChar x = (isLetter x) || (x == '_')


commentPrefix :: RE Char String
commentPrefix = "/*"

commentSuffix :: String -> RE Char String
commentSuffix _ = many anySym *> "*/"
