module Main where


import System.Console.Haskeline 
import Control.Monad.Except

import qualified LLVM.AST as ASTL
import Parser
import Lexer
import Emit

firstModule :: ASTL.Module
firstModule = ASTL.defaultModule { ASTL.moduleName = "First Module" }

main :: IO ()
main = repl

eval :: ASTL.Module -> String -> IO (Maybe ASTL.Module)
eval mod source = do
  let res  = (parse moduleParser "Term" source)
  case res of 
    Left err -> print err >> return Nothing
    Right ex -> do 
      print ex
      ast <- moduleGen mod ex
      return $ Just ast

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


liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

