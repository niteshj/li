import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos

data Tok   = Operand Int 
           | OperatorPlus
           | OperatorMinus
           | OpenParen
           | CloseParen deriving Show

data Exp = Eplus Exp Exp
         | Eminus Exp Exp
         | Eparen Exp
         | Econs Int deriving Show

type Token  = (SourcePos,Tok)

type MyParser a   = GenParser Token () a

mytoken :: (Tok -> Maybe a) -> MyParser a
mytoken test
  = token showToken posToken testToken
  where
    showToken (pos,tok)   = show tok
    posToken  (pos,tok)   = pos
    testToken (pos,tok)   = test tok


operand :: MyParser Int
operand = mytoken (\tok -> case tok of
                              Operand n  -> Just n
                              _          -> Nothing )

openparen :: MyParser ()
openparen = mytoken (\tok -> case tok of
                               OpenParen  -> Just ()
                               _          -> Nothing )

closeparen :: MyParser ()
closeparen = mytoken (\tok -> case tok of
                              CloseParen  -> Just ()
                              _          -> Nothing )

operatorPlus :: MyParser ()
operatorPlus = mytoken (\tok -> case tok of
                                  OperatorPlus  -> Just ()
                                  _             -> Nothing )

operatorMinus :: MyParser ()
operatorMinus = mytoken (\tok -> case tok of
                                   OperatorMinus  -> Just ()
                                   _              -> Nothing )


exprParser :: MyParser Exp
exprParser = try plusExprParser  
             <|> try minusExprParser 
             <|> try parenExprParser
             <|> constExprParser
             

plusExprParser  = do operatorPlus
                     e1 <- exprParser
                     e2 <- exprParser
                     return $ Eplus e1 e2

minusExprParser  = do operatorMinus
                      e1 <- exprParser
                      e2 <- exprParser
                      return $ Eminus e1 e2

parenExprParser  = do openparen
                      e1 <- exprParser
                      closeparen
                      return $ Eparen e1

constExprParser = do n <- operand
                     return $ Econs n

evalExpr :: Exp -> Int
evalExpr (Econs n) = n
evalExpr (Eplus e1 e2) = (evalExpr e1) + (evalExpr e2)
evalExpr (Eminus e1 e2) = (evalExpr e1) - (evalExpr e2)
evalExpr (Eparen e1) = evalExpr e1

pos = newPos "hi" 1 2 
op = [(pos, OperatorMinus), (pos, Operand 7), (pos, OpenParen), (pos, OperatorPlus), (pos, Operand 2), (pos, Operand 3), (pos, CloseParen)]                                 
