module Main where 

import Eval.Eval
import Lexer.Lexer
import Parser.SParser
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Prim
import Text.ParserCombinators.Parsec.Pos
import System.Console.Readline

import System
import System.IO
import Control.Monad.State
import Control.Monad.Error


main = do args <- getArgs
          case length args of
            0           -> runErrorT (evalStateT repl initialCtx)
            1           -> do program <- readFile (args !! 0)
                              runErrorT (evalStateT (evalString program) initialCtx)
            otherwise   -> do putStrLn $ "Usage is :: li [fileName]"
                              return $ Right ()
  



repl :: StateT Context SError ()
repl = do 
  liftIO $ hFlush stdout
  line <- liftIO $ readline "li > "
  case line of
    Nothing  -> return ()
    Just "(quit)" -> return ()
    Just line   -> do liftIO $ addHistory line
                      evalString line
                      repl
                      

evalString string = do let scannedTokens = alexScanTokens string
                       if (null scannedTokens)
                         then return ()
                         else do let programs = parse programParser "liParser" scannedTokens
                                 evaledProgram <- case programs of
                                                    Left err   -> throwError "Parse error"
                                                    Right prgms -> mapM eval prgms
                                 let evaledProgramOutput = map show evaledProgram
                                 if (null evaledProgramOutput)  
                                    then return ()
                                    else liftIO $ putStr $ unlines $ filter (/= "") evaledProgramOutput 
                             
                       `catchError` (\e -> do liftIO $ putStrLn e)
                                              


