{
module Lexer.Lexer  where

import Text.ParserCombinators.Parsec.Pos
import Data.List as L
}


%wrapper "posn"



@whitespace 	      	= " "  | \n | \t

@comment 	   	= \;.* | \;.*\n

@atmosphere 	   	= @whitespace | @comment

@interTokenSpace   	= @atmosphere+

@delimeter 	     	= @whitespace 	               	|
		   	  \( | \) | \" | \;

$letter 	   	= [a-zA-Z]
$specialInitial    	= [!\$\%&\*\/:\<=\>\?\^_\~]

@initial 	   	= $letter | $specialInitial

$digit 		     	= [0-9]

$specialSubsequent	= [\+\-\.\@]

@subsequent 	   	= @initial | $digit | $specialSubsequent

@peculiarIdentifier	= \+ | \- | \.\.\. 

@identifier 	   	= @initial @subsequent* | @peculiarIdentifier 

@expressionKeyword 	= [qQ][uU][oO][tT][eE] | [lL][aA][mM][bB][dD][aA] | [sS][eE][tT]! | 
			  [bB][eE][gG][iI][nN] | [cC][oO][nN][dD] | [aA][nN][dD] | [oO][rR] | [cC][aA][sS][eE] |
			  [lL][eE][tT] | [lL][eE][tT]\* | [lL][eE][tT][rR][eE][cC] | [dD][oO] | [dD][eE][lL][aA][yY] |
			  [qQ][uU][aA][sS][iI][qQ][uU][oO][tT][eE]

@syntacticKeyword 	= @expressionKeyword | [eE][lL][sS][eE] | => | [dD][eE][fF][iI][nN][eE] | 
			  [uU][nN][qQ][uU][oO][tT][eE] | [uU][nN][qQ][uU][oO][tT][eE]\-[sS][pP][lL][iI][cC][iI][nN][gG]

@variable		= @identifier 		       --To be corrected. variable = set identifier - set syntacticKeyword

@boolean 		= \#[tTfF]

@characterName 		= space | newline

@character		= \#\\ [\32-\126] | \#\\ @characterName


@stringElement		= [^\"\\] | \\\" | \\\\
@string 		= \"@stringElement*\"





--Patterns for Numbers Begin

$digit2          = 0-1
$digit8      	 = 0-7
$digit10     	 = $digit
$digit16   	 = [0-9a-fA-F]

@radix2      	 = \#[bB]
@radix8      	 = \#[oO]
@radix10     	 = () | \#[dD]
@radix16     	 = \#[xX]

@exactness   	 = () | \#[iI] | \#[eE]
@sign        	 = () | \+ | \-
$exMarker    	 = [eEsSfFdDlL] 
@suffix      	 = () | $exMarker @sign $digit10+

@prefix2     	 = @radix2  @exactness | @exactness @radix2
@prefix8     	 = @radix8  @exactness | @exactness @radix8
@prefix10    	 = @radix10 @exactness | @exactness @radix10
@prefix16    	 = @radix16 @exactness | @exactness @radix16
  
@uninteger2	 = $digit2+  \#*
@uninteger8	 = $digit8+  \#*
@uninteger10	 = $digit10+ \#*
@uninteger16	 = $digit16+ \#*

@decimal10 	 = @uninteger10 @suffix			| 
		   \. $digit10+ \#* @suffix		|
		   $digit10+ \. $digit10* \#* @suffix	|
		   $digit10+ \#+ \. \#* @suffix		


@ureal2 	 = @uninteger2  | @uninteger2  \/ @uninteger2
@ureal8 	 = @uninteger8  | @uninteger8  \/ @uninteger8
@ureal10 	 = @uninteger10 | @uninteger10 \/ @uninteger10 | @decimal10
@ureal16 	 = @uninteger16 | @uninteger16 \/ @uninteger16

@real2 		 = @sign @ureal2
@real8 		 = @sign @ureal8
@real10		 = @sign @ureal10
@real16		 = @sign @ureal16

@complex2 	 = @real2			|
		   @real2 \@ @real2		|
		   @real2 \+ @ureal2 i		|
		   @real2 \- @ureal2 i		|
		   @real2 \+ i	      		|	 
		   @real2 \- i	      		|
		   \+ @ureal2 i			|
		   \- @ureal2 i			|
		   \+ i	       			|
		   \- i				

@complex8 	 = @real8			|
		   @real8 \@ @real8		|
		   @real8 \+ @ureal8 i		|
		   @real8 \- @ureal8 i		|
		   @real8 \+ i	      		|	 
		   @real8 \- i	      		|
		   \+ @ureal8 i			|
		   \- @ureal8 i			|
		   \+ i	       			|
		   \- i				

@complex10 	 = @real10			|
		   @real10 \@ @real10		|
		   @real10 \+ @ureal10 i	|
		   @real10 \- @ureal10 i	|
		   @real10 \+ i	      		|	 
		   @real10 \- i	      		|
		   \+ @ureal10 i		|
		   \- @ureal10 i		|
		   \+ i	       			|
		   \- i				

@complex16 	 = @real16			|
		   @real16 \@ @real16		|
		   @real16 \+ @ureal16 i	|
		   @real16 \- @ureal16 i	|
		   @real16 \+ i	      		|	 
		   @real16 \- i	      		|
		   \+ @ureal16 i		|
		   \- @ureal16 i		|
		   \+ i	       			|
		   \- i				


@num2		 = @prefix2  @complex2
@num8		 = @prefix8  @complex8
@num10		 = @prefix10 @complex10
@num16		 = @prefix16 @complex16


@number 	 = @num2 | @num8 | @num10 | @num16

--Patterns for Number Ends


@token			= @identifier			| 
		     	  @boolean 			| 
		     	  @number 		 	| 
		     	  @character 			| 
		     	  @string  			|
		     	  \( | \) | \#\( | \' | ` | \, | \,\@ | \.





tokens :-

@interTokenSpace			;
@syntacticKeyword			{ \p s -> (fromAlexPosn p, SyntacticKeyWord s)			}
@identifier				{ \p s -> (fromAlexPosn p, Identifer s)				}
@boolean				{ \p s -> if (s == "#t") || (s == "#T")
					       	  then (fromAlexPosn p, Boolean True)
						  else (fromAlexPosn p, Boolean False)  
													}
@number					{ \p s -> (fromAlexPosn p, Number  (toInt s))			}
@character				{ \p s -> (fromAlexPosn p, Character $ handleCharacter s)	}
@string					{ \p s -> (fromAlexPosn p, SString s)  		       		}

\(					{ \p s -> (fromAlexPosn p, OpenParen)				}
\)					{ \p s -> (fromAlexPosn p, CloseParen)				}
\'					{ \p s -> (fromAlexPosn p, SingleQuote)				}
\,					{ \p s -> (fromAlexPosn p, Comma) 				}
\.					{ \p s -> (fromAlexPosn p, Dot)        				}


{
-- Each right-hand side has type :: AlexPosn -> String -> Tok

-- Some action helpers:

-- The token type:
type Token   = (SourcePos, Tok)


fromAlexPosn :: AlexPosn -> SourcePos
fromAlexPosn (AlexPn _ line col) = newPos "" line col

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


data SNumber = SInt Int deriving (Show, Eq)

toInt :: String -> SNumber
toInt str = SInt (read str)

handleCharacter :: String -> Char
handleCharacter str | "space" `L.isSuffixOf` str   = ' '
		    | "newline" `L.isSuffixOf` str = '\n'
		    | otherwise		           = last str


}
