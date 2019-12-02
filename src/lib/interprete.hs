{-# LANGUAGE OverloadedStrings #-}

-- https://www.cs.princeton.edu/~appel/modern/ml/chap1/

module Interprete  where




type Id = String

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
parse _ = []
