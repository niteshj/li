module Parser.SParser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Prim
import Text.ParserCombinators.Parsec.Pos
import Lexer.Lexer


type Program = CmdOrDef

data CmdOrDef = CCommand Command
              | CDefinition Definition
                deriving (Eq, Ord)   


type Command = Exp

data Exp = EVariable Variable
         | ELiteral Literal
         | EPCall ProCall
         | ELambda Formals Body
         | ENone                     -- We need this only for eval
           deriving (Eq, Ord)   
                

type Variable = String

data Literal = LBool Bool
             | LNum Int
             | LChar Char
             | LString String
               deriving (Eq, Ord)   

data ProCall = ProCall {  operator :: Exp, 
                          operands :: [Exp] 
                       }  deriving (Eq, Ord)   

type Formals = [Variable] 

data Body = Body Definitions Sequence            deriving (Eq, Ord)   

type Definitions = [Definition]

data Definition = Define1 Variable Exp
                | Define2 Variable DefFormals Body
                | Define3 Definitions
                  deriving (Eq, Ord)   

type DefFormals = [Variable]

type Sequence = [Exp]



-- Parser starts here
type MyParser a   = GenParser Token () a

mytoken :: (Tok -> Maybe a) -> MyParser a
mytoken test = token showToken posToken testToken 
    where showToken (pos,tok)   = show tok
          posToken  (pos,tok)   = pos
          testToken (pos,tok)   = test tok


syntacticKeywordParser :: MyParser String
syntacticKeywordParser = mytoken (\tok -> case tok of
                                            SyntacticKeyWord str -> Just str
                                            _                    -> Nothing )

identiferParser :: MyParser String
identiferParser = mytoken (\tok -> case tok of
                                     Identifer str -> Just str
                                     _             -> Nothing )
 
booleanParser :: MyParser Bool
booleanParser = mytoken (\tok -> case tok of
                                   Boolean bool  -> Just bool
                                   _             -> Nothing )

numberParser :: MyParser SNumber
numberParser = mytoken (\tok -> case tok of
                                  Number num -> Just num
                                  _          -> Nothing )

characterParser :: MyParser Char
characterParser = mytoken (\tok -> case tok of
                                     Character c -> Just c
                                     _           -> Nothing )


sstringParser :: MyParser String
sstringParser =  mytoken (\tok -> case tok of
                                    SString str   -> Just str
                                    _             -> Nothing )

openParenParser :: MyParser ()
openParenParser = mytoken (\tok -> case tok of
                                     OpenParen -> Just ()
                                     _         -> Nothing )

closeParenParser :: MyParser ()
closeParenParser = mytoken (\tok -> case tok of
                                      CloseParen -> Just ()
                                      _         -> Nothing )
singleQuoteParser :: MyParser ()
singleQuoteParser = mytoken (\tok -> case tok of
                                       SingleQuote -> Just ()
                                       _           -> Nothing )

commaParser :: MyParser ()
commaParser = mytoken (\tok -> case tok of
                                       Comma -> Just ()
                                       _     -> Nothing )

dotParser :: MyParser ()
dotParser = mytoken (\tok -> case tok of
                                       Dot -> Just ()
                                       _     -> Nothing )

fromSNumber :: SNumber -> Int
fromSNumber (SInt n) = n

-- Literal parser
literalParser :: MyParser Literal
literalParser = try lBoolParser
                <|> try lNumParser
                <|> try lCharParser
                <|> lStringParser

lBoolParser :: MyParser Literal
lBoolParser = do bool <- booleanParser
                 return $ LBool bool

lNumParser :: MyParser Literal
lNumParser = do numS <- numberParser
                let num = fromSNumber numS
                return $ LNum num

lCharParser :: MyParser Literal
lCharParser = do c <- characterParser
                 return $ LChar c
 
lStringParser :: MyParser Literal
lStringParser = do str <- sstringParser
                   return $ LString str


-- Exp parser
-- < variable > = < identifer >   -- accn to our lexer
variableParser :: MyParser Variable
variableParser = identiferParser

formalsParser :: MyParser [Variable]
formalsParser = try (do openParenParser
                        listVars <- many variableParser
                        closeParenParser
                        return listVars)

                <|> (do var <- variableParser
                        return [var])
                   
defFormalsParser :: MyParser [Variable]
defFormalsParser = do listVars <- many variableParser
                      return listVars

bodyParser :: MyParser Body
bodyParser = do defs <- definitionsParser
                seq  <- sequenceParser
                return $ Body defs seq

-- hack :: TODOC
sequenceParser :: MyParser [Exp]
sequenceParser = many1 expParser

commandParser :: MyParser Exp
commandParser = expParser

-- case sensitivity of keywords assumed to be handled by lexer
definitionParser :: MyParser Definition
definitionParser = try (do openParenParser
                           "define" <- syntacticKeywordParser
                           var <- variableParser
                           exp <- expParser
                           closeParenParser
                           return $ Define1 var exp)

                   <|> try (do openParenParser
                               "define" <- syntacticKeywordParser
                               openParenParser
                               var <- variableParser
                               defFormals <- defFormalsParser
                               closeParenParser
                               body <- bodyParser
                               closeParenParser
                               return $ Define2 var defFormals body)

                   <|> (do openParenParser
                           "begin" <- syntacticKeywordParser
                           defs <- definitionsParser
                           return $ Define3 defs)

definitionsParser :: MyParser Definitions                       
definitionsParser = many $ try definitionParser         

proCallParser :: MyParser ProCall
proCallParser = do openParenParser
                   operator <- expParser
                   operands <- many expParser
                   closeParenParser
                   return $ ProCall operator operands

expParser :: MyParser Exp
expParser = try (do var <- variableParser
                    return $ EVariable var)

            <|> try (do literal <- literalParser
                        return $ ELiteral literal)

            <|> try (do pCall <- proCallParser
                        return $ EPCall pCall)
            <|> lambdaParser

lambdaParser :: MyParser Exp
lambdaParser = do openParenParser
                  "lambda" <- syntacticKeywordParser
                  formals  <- formalsParser
                  body     <- bodyParser
                  closeParenParser
                  return $ ELambda formals body

programParser :: MyParser [Program]
programParser = many cmdOrDefParser
                
cmdOrDefParser :: MyParser CmdOrDef
cmdOrDefParser = try (do cmd <- commandParser
                         return $ CCommand cmd)

                 <|> (do def <- definitionParser
                         return $ CDefinition def)



instance Show Exp where
    show (EVariable var)    = var
    show (ELiteral literal) = show literal
    show (EPCall procedure) = show procedure
    show (ELambda _ _)      = "#<procedure>"
    show (ENone)            = ""

instance Show Literal where
    show (LBool b)     = if b == True then "#t" else "#f"
    show (LNum num)       = show num
    show (LString string) = string

instance Show ProCall where
    show _ = "#<procedure>"

