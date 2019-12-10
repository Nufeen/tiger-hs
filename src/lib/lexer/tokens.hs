module Tokens where

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
