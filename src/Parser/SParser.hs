module Parser.SParser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Prim
import Text.ParserCombinators.Parsec.Pos
import Lexer.Lexer

data Datum = SDatum SimpleDatum 

data SimpleDatum = SDBoolean Bool
                 | SDNumber Int
                 | SDChar Char
                 | SDString String
                 | SDIdentifier String

data Exp = EVariable String
         | ELiteral Literal
         | EPCall ProCall
         | ELambda Formals Body

data Literal = LBool Bool
             | LNum Int
             | LChar Char
             | LString String

data ProCall = ProCall { operator :: Exp, operands :: [Exp] }

type Formals = [String] 

data Body = Body Definitions Sequence

data Sequence = Sequence [Command] Exp

type Command = Exp


-- Program 
type Program = CmdOrDef

data CmdOrDef = CCommand Command
              | CDefinition Definition
              | CBegin CmdOrDef [CmdOrDef]

data Definition = Define1 Variable Exp
                | Define2 Variable DefFormals Body
                | Define3 Definitions

type Definitions = [Definition]

type Variable = String

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


{-

data Tok   = SyntacticKeyWord String		| 
     	     Identifer        String         	| 
     	     Boolean  	      Bool  	     	| 
	     Number  	      SNumber 	     	| 
	     Character        Char 	     	| 
	     SString  	      String		|
	     OpenParen				|
	     CloseParen				|
	     SingleQuote			|
	     Comma				|
	     Dot				
	     deriving (Eq,Show)

type Token   = (SourcePos, Tok)

fromAlexPosn :: AlexPosn -> SourcePos
fromAlexPosn (AlexPn _ line col) = newPos "" line col

-}
  



