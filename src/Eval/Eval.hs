module Eval.Eval where

import Parser.SParser
import Text.ParserCombinators.Parsec
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Error



-- Context in which expressions will be evaluated
type SymbolTable = Map.Map String Exp
data Context = Ctx SymbolTable (Maybe Context)

-- A state monad that holds a context and an evaluation result
type SError = ErrorT String IO
type SResult = StateT Context SError Exp


-- Helper context functions
updateSymbol s eval_e = modify (\(Ctx sym_table parentCtx)->(Ctx (Map.insert s eval_e sym_table)) parentCtx)

updateSymbolInParent s eval_e = modify (\(Ctx sym_table parent_ctx)->(Ctx sym_table (updatedCtx parent_ctx)))
    where updatedCtx (Just (Ctx sym_table ctx)) = (Just (Ctx (Map.insert s eval_e sym_table) ctx))

pushContext ctx = Ctx Map.empty (Just ctx)

popContext ctx@(Ctx _ Nothing) = ctx
popContext (Ctx _ (Just parentCtx)) = parentCtx


initialCtx = Ctx (Map.singleton "initial" ENone) Nothing


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

-- Comes last in the eval
eval _ = throwError "bas karo bhai"


{-

eval (BlaiseSymbol s) = do context <- get
                           lookupSymbol context
    where lookupSymbol (Ctx sym_table parentCtx) =
              if s `Map.member` sym_table == True
              then return (sym_table Map.! s)
              else case parentCtx of
                     Nothing -> throwError ("Symbol " ++ s ++ " is unbound.")
                     (Just parent) -> lookupSymbol parent

type Program = CmdOrDef

data CmdOrDef = CCommand Command
              | CDefinition Definition
                deriving Show

type Command = Exp

data Exp = EVariable Variable
         | ELiteral Literal
         | EPCall ProCall
         | ELambda Formals Body
           deriving Show

type Variable = String

data Literal = LBool Bool
             | LNum Int
             | LChar Char
             | LString String
               deriving Show

data ProCall = ProCall { operator :: Exp, operands :: [Exp] }
             deriving Show

type Formals = [Variable] 

data Body = Body Definitions Sequence
            deriving Show

type Definitions = [Definition]

data Definition = Define1 Variable Exp
                | Define2 Variable DefFormals Body
                | Define3 Definitions
                  deriving Show

type DefFormals = [Variable]

type Sequence = [Exp]


-- Datum definitions start

data Datum = SDatum SimpleDatum 

data SimpleDatum = SDBoolean Bool
                 | SDNumber Int
                 | SDChar Char
                 | SDString String
                 | SDIdentifier String 


-}



