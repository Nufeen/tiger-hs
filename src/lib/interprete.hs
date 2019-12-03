{-# LANGUAGE OverloadedStrings #-}

-- https://www.cs.princeton.edu/~appel/modern/ml/chap1/

module Interprete where

import Data.List (find)
import Data.Maybe (fromJust)

type Id = String

type  Table = [(Id, Int)]

lookup' :: Id  -> Table -> Int
lookup' id = fromJust . lookup id

data BinOp = Plus | Minus | Times | Divide
              deriving Show

data Statement = CompoundStm Statement Statement
               | AssignStm Id Expression
               | PrintStm [Expression]
              deriving Show

data Expression = IdExp Id
                | NumExp Int
                | OpExp Expression BinOp Expression
                | EseqExp Statement Expression
              deriving Show

-- test program:
-- a := 5 + 3 ;
-- b := ( print ( a, a - 1) , 10 * a) ;
-- print (b)
program :: Statement
program = CompoundStm
            (AssignStm "a" (OpExp (NumExp 5) Plus (NumExp 3)))
            (CompoundStm
                (AssignStm "b"
                    (EseqExp
                        (PrintStm [IdExp "a", OpExp (IdExp "a") Minus (NumExp 1)])
                        (OpExp (NumExp 10) Times (IdExp "a"))))
                (PrintStm [IdExp "b"]))

-- maximum number of arguments of any print statement within
maxArgs :: Statement -> Int
maxArgs s =  maximum $ map (\(PrintStm e) -> length e) $ parse s

parseE :: Expression -> [Statement]
parseE (EseqExp s e) = concat [parse s, parseE e]
parseE (OpExp a _ b) =  parseE a ++ parseE b
parseE _ = []

parse :: Statement ->  [Statement]
parse ( CompoundStm a b)  =  parse a ++ parse b
parse (AssignStm _ e) = parseE e
parse (PrintStm x) =  [PrintStm x]  ++  concat (map parseE x)

-- table interpretations, pure version w/o print
interpStm :: Statement -> Table -> Table
interpStm ( CompoundStm a b) t = interpStm b (interpStm a t)
interpStm (AssignStm id e) t = (id, k) : t
  where (k, _) = interpExp e t
interpStm  (PrintStm _) t = t

interpExp :: Expression -> Table -> (Int , Table)
interpExp (IdExp id) t = (lookup' id t, t)
interpExp (NumExp n) t = (n, t)
interpExp (OpExp a op b) t =
  case op of
    Plus -> (i + j, t2)
    Minus -> (i - j, t2)
    Times -> (i * j, t2)
    Divide -> (i `div` j, t2)
  where
    (i, t1) =  interpExp a t
    (j, t2) =  interpExp b t1
interpExp (EseqExp s e) t = interpExp e (interpStm s t)

test = interpStm program []
