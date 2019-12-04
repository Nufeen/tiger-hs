{-# LANGUAGE OverloadedStrings #-}

-- https://www.cs.princeton.edu/~appel/modern/ml/chap2/

module Lexer where

import System.Environment
import System.Directory
import System.Console.Pretty
import Control.Exception (try)
import Language.Lexer.Applicative
import Text.Regex.Applicative (RE)
import Text.Regex.Applicative (psym, some, many, anySym, (<|>))
import Data.Char (isSpace, isDigit, isLetter)

data Token =
    TokenWHILE
  | TokenFOR
  | TokenTO
  | TokenBREAK
  | TokenLET
  | TokenIN
  | TokenEND
  | TokenFUNCTION
  | TokenVAR
  | TokenTYPE
  | TokenARRAY
  | TokenIF
  | TokenTHEN
  | TokenELSE
  | TokenDO
  | TokenOF
  | TokenNIL
  | TokenCOMMA
  | TokenCOLON
  | TokenSEMICOLON
  | TokenLPAREN
  | TokenRPAREN
  | TokenLBRACK
  | TokenRBRACK
  | TokenLBRACE
  | TokenRBRACE
  | TokenDOT
  | TokenPLUS
  | TokenMINUS
  | TokenTIMES
  | TokenDIVIDE
  | TokenEQ
  | TokenNEQ
  | TokenLT
  | TokenLE
  | TokenGT
  | TokenGE
  | TokenAND
  | TokenOR
  | TokenASSIGN
  | TokenSTRING
  | TokenINT
  deriving (Eq, Show)

tigerToken :: RE Char Token
tigerToken =
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
  <|> TokenSTRING     <$ (some $ psym isLetter)
  <|> TokenINT        <$ (some $ psym isDigit)

lexer :: Lexer Token
lexer = mconcat
  [ token      (longest tigerToken)
  , whitespace (longest $ psym isSpace)
  , whitespace (longestShortest commentPrefix commentSuffix)
  ]

commentPrefix :: RE Char String
commentPrefix = "/*"

commentSuffix :: String -> RE Char String
commentSuffix _ = many anySym *> "*/"

tryL :: IO a -> IO (Either LexicalError a)
tryL = try

test :: IO ()
test = do
  dir <- listDirectory "testcases"
  let files = filter (\x -> head x /= '.') dir
  mapM_ lex files
  where
    lex file = do
      fileContent <- readFile $ "testcases" ++ "/" ++ file
      putStrLn $ color Yellow file
      res <-
        tryL $
        putStrLn $
        show $
        streamToList $
        runLexer lexer file fileContent
      case res of
        Left e -> putStrLn $ color Red $ "\n" ++ (show e) ++ "\n"
        Right _ -> putStrLn "\n"
