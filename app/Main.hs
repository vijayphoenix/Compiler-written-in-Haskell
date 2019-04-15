module Main where

import Lexer

main :: IO ()
main = do
    str <- getLine
    putStrLn ("Input:" ++ str)
    print $ parse ident "File1" str
    main 
    -- print $ parse ident "File1" str
