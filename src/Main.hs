module Main where 

import Eval.Eval
import Lexer.Lexer
import Parser.SParser
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Prim
import Text.ParserCombinators.Parsec.Pos
import System.Console.Readline

import System.IO
import Control.Monad.State
import Control.Monad.Error




main = do 
  runErrorT (evalStateT repl initialCtx)

repl :: StateT Context SError ()
repl = do 
  liftIO $ hFlush stdout
  line <- liftIO $ readline "li > "
  case line of
    Nothing  -> return ()
    Just "(quit)" -> return ()
    Just line   -> do liftIO $ addHistory line
                      let program = parse programParser "fail" (alexScanTokens line)
                      evaledProgram <- case program of
                                         Left err   -> throwError "Parse error"
                                         Right prgm -> eval prgm
                      let evaledProgramOutput = show evaledProgram
                      if (null evaledProgramOutput)  
                         then return ()
                         else liftIO $ putStrLn $ "li > " ++ evaledProgramOutput -- evaledProgram :: Exp
                             
                      repl        

                  `catchError` (\e -> do liftIO $ putStrLn e 
                                         repl)

        
    