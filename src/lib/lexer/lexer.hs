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
import Data.Char (isSpace, isDigit, isLetter, isAlphaNum)
import Data.List

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
  | TokenSTRING String
  | TokenINT Int
  | TokenID String
  deriving (Eq, Show)

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

lexer :: Lexer Token
lexer = mconcat
  [ token      (longest lexeme)
  , whitespace (longest $ psym isSpace)
  , whitespace (longestShortest commentPrefix commentSuffix)
  ]

commentPrefix :: RE Char String
commentPrefix = "/*"

commentSuffix :: String -> RE Char String
commentSuffix _ = many anySym *> "*/"

test :: IO ()
test = do
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
