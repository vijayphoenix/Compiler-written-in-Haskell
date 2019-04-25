module Main where


import System.Console.Haskeline 
import Control.Monad.Except

import qualified LLVM.AST as AST
import Parser
import Lexer

firstModule :: AST.Module
firstModule = AST.defaultModule { AST.moduleName = "First Module" }

main :: IO ()
main = repl

eval :: AST.Module -> String -> IO (Maybe AST.Module)
eval mod source = do
  let res  = (parse moduleParser "Term" source)
  case res of 
    Left err -> print err >> return Nothing
    Right ex -> do 
      print ex
      return $ Just mod

repl :: IO ()
repl = runInputT defaultSettings (loop firstModule)
  where 
  loop mod = do 
    inputCode <- getInputLine "Haskull> "
    case inputCode of 
      Nothing -> outputStrLn "Exiting Haskull."
      Just input -> do
        wrappedMod <- liftIO $ eval mod input
        case wrappedMod of 
            Just modn -> loop modn
            Nothing   -> loop mod
      -- Just input -> do
      --   let tree = (parse commandParser "Term" input) in 
      --     loop mod
        -- case (parse commandParser "sdf" inputCode) of 
        --   Left mess -> outputStrLn mess
        --   Right out -> outputStrLn out
        -- outputStrLn (parse commandParser "sdf" inputCode)
        -- repl

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

