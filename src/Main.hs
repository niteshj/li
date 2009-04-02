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
  line <- liftIO $ readline "> "
  case line of
    Nothing  -> return ()
    Just "(quit)" -> return ()
    Just line   -> do liftIO $ addHistory line
                      let program = parse programParser "fail" (alexScanTokens line)
                      evaledProgram <- case program of
                                         Left err   -> throwError "error"
                                         Right prgm -> eval prgm
                      liftIO $ putStrLn $ show evaledProgram -- evaledProgram :: Exp
                      repl        

                  `catchError` (\e -> do liftIO $ putStrLn e 
                                         repl)



-- evalTest :: String -> Datum
-- evalTest prg = case parse programParser "fail" (alexScanTokens prg) of 
--                  Left err   -> (SDatum $ SDString $ "Parse error:\n" ++ (show err))
--                  Right prgm -> eval prgm

-- program = "nite"

-- main = runErrorT (evalStateT readEvalPrintLoop initialCtx)

-- --main = putStrLn $ show $ evalTest program

-- readEvalPrintLoop :: IO ()
-- readEvalPrintLoop = do
--   maybeLine <- liftIO $ readline "li > "
--   case maybeLine of 
--     Nothing     -> return () -- EOF / control-d
--     Just "exit" -> return ()
--     Just line -> do addHistory line
--                     liftIO $ putStrLn $ "The user input: " ++ (show line)
--                     readEvalPrintLoop

