module Main where 

import Eval.Eval
import Lexer.Lexer
import Parser.SParser
import System
import System.IO
import Control.Monad.State
import Control.Monad.Error
import System.Console.Readline
import Text.ParserCombinators.Parsec

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
  newLine <- liftIO $ isLoadFile line
  case newLine of
    Nothing            -> return ()
    Just "(quit)"      -> return ()
    Just "error::load" -> do liftIO $ putStrLn "Usage: load <filename>"
                             repl
    Just "repl"        -> repl
    Just line          -> do liftIO $ addHistory line
                             evalString line
                             repl
                      

evalString :: String -> StateT Context SError ()
evalString string = do let scannedTokens = alexScanTokens string
                       if (null scannedTokens)
                         then return ()
                         else do let programs = parse programParser "liParser" scannedTokens
                                 evaledProgram <- case programs of
                                                    Left err    -> throwError "Parse error"
                                                    Right prgms -> mapM eval prgms
                                 let evaledProgramOutput = map show evaledProgram
                                 if (null evaledProgramOutput)  
                                    then return ()
                                    else liftIO $ putStr $ unlines $ filter (/= "") evaledProgramOutput 
                             
                       `catchError` (\e -> do liftIO $ putStrLn e)
                                              

isLoadFile (Just [])          = return $ Just "repl"

isLoadFile (Just loadCommand) = do let args = words loadCommand
                                   case (args !! 0) of 
                                     "load"    -> if length args /= 2 
                                                  then return $ Just "error::load"
                                                  else do fileRead <- readFile (args !! 1)
                                                          addHistory loadCommand
                                                          putStrLn $ "File Loaded :: " ++ (args !! 1)            
                                                          return $ Just fileRead
                                     otherwise -> return $ Just loadCommand 
                                   
isLoadFile Nothing = return $ Nothing