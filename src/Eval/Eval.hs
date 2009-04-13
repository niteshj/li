module Eval.Eval where

import Parser.SParser
import Text.ParserCombinators.Parsec
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Error
import Data.List


-- Context in which expressions will be evaluated
type SymbolTable = Map.Map String Exp
data Context     = Ctx SymbolTable (Maybe Context)

-- A state monad that holds a context and an evaluation result
type SError  = ErrorT String IO
type SResult = StateT Context SError Exp


-- Helper context functions
updateSymbol s eval_e = modify (\(Ctx sym_table parentCtx)->(Ctx (Map.insert s eval_e sym_table)) parentCtx)

pushContext ctx = Ctx Map.empty (Just ctx)

popContext ctx@(Ctx _ Nothing)      = ctx
popContext (Ctx _ (Just parentCtx)) = parentCtx


initialCtx = Ctx (Map.fromList 
                              [("+", (EVariable "+")),
                               ("-", (EVariable "-")),
                               ("*", (EVariable "*")),
                               ("/", (EVariable "/")),
                               ("expt", (EVariable "expt")),
                               ("modulo", (EVariable "modulo")),
                               ("=", (EVariable "=")),
                               ("<", (EVariable "<")),
                               (">", (EVariable ">")),
                               ("<=", (EVariable "<=")),
                               (">=", (EVariable ">=")),
                               ("if", (EVariable "if"))
                              ])                               
                              Nothing


eval :: Program -> StateT Context SError Exp
eval (CCommand (ELiteral (LNum n)))      = return $ ELiteral $ LNum n
eval (CCommand (ELiteral (LBool b)))     = return $ ELiteral $ LBool b
eval (CCommand (ELiteral (LChar c)))     = return $ ELiteral $ LChar c
eval (CCommand (ELiteral (LString str))) = return $ ELiteral $ LString str

eval (CDefinition (Define1 var exp)) = do evaledExp <- eval (CCommand exp)
                                          updateSymbol var evaledExp
                                          return $ ENone
                                          
eval (CCommand (EVariable var)) = do context <- get
                                     lookupSymbol context
    where lookupSymbol (Ctx sym_table parentCtx) =
              if var `Map.member` sym_table == True
              then return (sym_table Map.! var)
              else case parentCtx of
                     Nothing -> throwError ("Symbol " ++ var ++ " is undefined")
                     (Just parent) -> lookupSymbol parent

eval (CCommand (ELambda formals  body)) = return $ ELambda formals body
 
-- To evaluate ProCall
-- First get the operator and operands
-- We evaluate the operator, the result of which should be a lambda expression else error
-- Then lazily evaluate the arguments, lazily to take care or recursion
-- apply lambda expression on the body
eval (CCommand (EPCall procedure)) = do let fun  = operator procedure
                                            args = operands procedure
                                        lambdaExpr <- eval $ CCommand fun
                                        apply lambdaExpr args

    where apply (ELambda formals (Body defs exps)) args = do evaledArgs <- mapM eval $ map CCommand args
                                                             let zippedArgs = zip formals evaledArgs
                                                             pushArgs zippedArgs
                                                             mapM eval (map CDefinition defs)
                                                             result <- mapM eval (map CCommand exps)
                                                             modify popContext
                                                             return $ last result
          apply (EVariable str) evaledArgs = evalLibraryFunction str evaledArgs
          apply _ _                        = throwError "strange function apply error"

          pushArgs zippedArgs = do modify pushContext
                                   mapM (uncurry updateSymbol) zippedArgs 
                                   
-- Comes last in the eval
eval _ = throwError "eval: no evaluation fits for this expression"


-- Base Library Support
evalLibraryFunction "+" evaledArgs = do args <- (mapM eval $ map CCommand evaledArgs)
                                        let numList = map (\(ELiteral (LNum num)) -> num) args
                                        return $ ELiteral $ LNum $ foldl1' (+) numList

evalLibraryFunction "-" evaledArgs = do args <- (mapM eval $ map CCommand evaledArgs)
                                        let numList = map (\(ELiteral (LNum num)) -> num) args
                                        return $ ELiteral $ LNum $ foldl1' (-) numList

evalLibraryFunction "*" evaledArgs = do args <- (mapM eval $ map CCommand evaledArgs)
                                        let numList = map (\(ELiteral (LNum num)) -> num) args
                                        return $ ELiteral $ LNum $ foldl1' (*) numList

evalLibraryFunction "/" evaledArgs = do let numList = map (\(ELiteral (LNum num)) -> num) evaledArgs
                                        return $ ELiteral $ LNum $ foldl1' (div) numList

evalLibraryFunction "expt" evaledArgs 
    | length evaledArgs == 2 = do args <- (mapM eval $ map CCommand evaledArgs)
                                  let [n1, n2] = map (\(ELiteral (LNum num)) -> num) args
                                  return $ ELiteral $ LNum $ n1 ^ n2
    | otherwise              = failWithErrorMessage "expt" "Takes two arguements" 


evalLibraryFunction "modulo" evaledArgs
    | length evaledArgs == 2 = do args <- (mapM eval $ map CCommand evaledArgs)
                                  let [n1, n2] = map (\(ELiteral (LNum num)) -> num) args
                                  return $ ELiteral $ LNum $ mod n1 n2
    | otherwise              = failWithErrorMessage "modulo" "Takes two arguements" 


evalLibraryFunction "=" evaledArgs 
    | length evaledArgs == 2 = do [n1, n2] <- (mapM eval $ map CCommand evaledArgs)
                                  return $ ELiteral $ LBool (n1 == n2)
    | otherwise              = failWithErrorMessage "=" "Currently takes two arguements" 

evalLibraryFunction "<" evaledArgs 
    | length evaledArgs == 2 = do [n1, n2] <- (mapM eval $ map CCommand evaledArgs)
                                  return $ ELiteral $ LBool (n1 < n2)
    | otherwise              = failWithErrorMessage "<" "Currently takes two arguements" 


evalLibraryFunction ">" evaledArgs
    | length evaledArgs == 2 = do [n1, n2] <- (mapM eval $ map CCommand evaledArgs)
                                  return $ ELiteral $ LBool (n1 > n2)
    | otherwise              = failWithErrorMessage ">" "Currently takes two arguements" 


evalLibraryFunction "<=" evaledArgs
    | length evaledArgs == 2 = do [n1, n2] <- (mapM eval $ map CCommand evaledArgs)
                                  return $ ELiteral $ LBool (n1 <= n2)
    | otherwise              = failWithErrorMessage "<=" "Currently takes two arguements" 

evalLibraryFunction ">=" evaledArgs
    | length evaledArgs == 2 = do [n1, n2] <- (mapM eval $ map CCommand evaledArgs)
                                  return $ ELiteral $ LBool (n1 >= n2)
    | otherwise              = failWithErrorMessage ">=" "Currently takes two arguements" 


-- For if then else
evalLibraryFunction "if" [boolExpr, yesExpr, noExpr] = do (ELiteral (LBool boolEvaled)) <- eval $ CCommand boolExpr
                                                          if boolEvaled == True 
                                                            then eval $ CCommand yesExpr 
                                                            else eval $ CCommand noExpr
-- For if then
evalLibraryFunction "if" [boolExpr, yesExpr] = do (ELiteral (LBool boolEvaled)) <- eval $ CCommand boolExpr
                                                  if boolEvaled == True 
                                                     then eval $ CCommand yesExpr 
                                                     else return ENone

failWithErrorMessage functionName errorString = fail $ functionName ++ " :: " ++ errorString