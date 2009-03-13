module Parser.SParser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Prim
import Text.ParserCombinators.Parsec.Pos
import Lexer.Lexer

data Datum = SDatum SimpleDatum 
             deriving Show

data SimpleDatum = SDBoolean Bool
                 | SDNumber Int
                 | SDChar Char
                 | SDString String
                 | SDIdentifier String 
                   deriving Show

data Exp = EVariable Variable
         | ELiteral Literal
         | EPCall ProCall
         | ELambda Formals Body
           deriving Show

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

-- TODOC
type Sequence = [Exp]

type Command = Exp

type Variable = String

-- Program 
type Program = CmdOrDef

data CmdOrDef = CCommand Command
              | CDefinition Definition
              | CBegin [CmdOrDef]
                deriving Show

data Definition = Define1 Variable Exp
                | Define2 Variable DefFormals Body
                | Define3 Definitions
                  deriving Show

type Definitions = [Definition]

type DefFormals = [Variable]



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

-- Parsers for SimpleDatum
simpleDatumParser :: MyParser SimpleDatum
simpleDatumParser = try sdBooleanParser
                    <|> try sdNumberParser
                    <|> try sdCharParser
                    <|> try sdStringParser
                    <|> sdIdentifierParser

sdBooleanParser :: MyParser SimpleDatum
sdBooleanParser = do bool <- booleanParser
                     return $ SDBoolean bool

sdNumberParser :: MyParser SimpleDatum
sdNumberParser =  do numS <- numberParser 
                     let num = fromSNumber numS
                     return $ SDNumber num


sdCharParser :: MyParser SimpleDatum
sdCharParser = do c <- characterParser
                  return $ SDChar c

sdStringParser :: MyParser SimpleDatum
sdStringParser = do str <- sstringParser
                    return $ SDString str

sdIdentifierParser :: MyParser SimpleDatum
sdIdentifierParser = do str <- identiferParser
                        return $ SDIdentifier str

-- Datum parser
datumParser :: MyParser Datum
datumParser = do simpleDatum <- simpleDatumParser
                 return $ SDatum simpleDatum


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
definitionsParser = many definitionParser         

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
                  return $ ELambda formals body

programParser :: MyParser Program
programParser = cmdOrDefParser

cmdOrDefParser :: MyParser CmdOrDef
cmdOrDefParser = try (do cmd <- commandParser
                         return $ CCommand cmd)

                 <|> try (do def <- definitionParser
                             return $ CDefinition def)
                 
                 <|> (do cmdOrDefs <- many cmdOrDefParser
                         return $ CBegin cmdOrDefs)


