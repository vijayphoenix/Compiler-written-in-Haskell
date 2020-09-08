-- import Lib
module Main where

import System.Console.Haskeline 
import Control.Monad.Except

import qualified LLVM.AST as ASTL
import Parser
import Lexer
import AST
import Emit

main :: IO()
main 
    |   func1 && 
        func2 && 
        func3 && 
        func4 &&
        func5 = putStrLn "\nTEST PASSING"
    | otherwise = error "TESTS NOT PASSING"
    
func1 = case (parse binOpCallStmtParser "File1" "5+3;") of
    Right s -> (show (parse binOpCallStmtParser "File1" "5+3;")) == "Right (BinOpCallStmt (BinOpCall {op = Plus, lhs = LiteralStmt (IntLiteral 5), rhs = LiteralStmt (IntLiteral 3)}))"
    Left err -> error $ show err

func2 = case (parse binOpCallStmtParser "File2" "x+y;") of
    Right s -> (show (parse binOpCallStmtParser "File2" "x+y;")) == "Right (BinOpCallStmt (BinOpCall {op = Plus, lhs = Var \"x\", rhs = Var \"y\"}))"
    Left err -> error $ show err

func3 = case (parse literalStmtParser "File2" "\"Hello\"") of
    Right s -> (show (parse literalStmtParser "File2" "\"Hello\"")) == "Right (LiteralStmt (StrLiteral \"Hello\"))"
    Left err -> error $ show err

func4 = case (parse literalStmtParser "File2" "15000") of
    Right s -> (show (parse literalStmtParser "File2" "15000")) == "Right (LiteralStmt (IntLiteral 15000))"
    Left err -> error $ show err

func5 = case (parse funcCallStmtParser "File2" "func(4,3)") of
    Right s -> (show (parse funcCallStmtParser "File2" "func(4,3)")) == "Right (FuncCallStmt (FuncCall {callee = \"func\", args = [LiteralStmt (IntLiteral 4),LiteralStmt (IntLiteral 3)]}))"
    Left err -> error $ show err
