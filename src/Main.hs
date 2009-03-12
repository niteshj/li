module Main where 

import Lexer.Lexer
import Parser.SParser
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Prim
import Text.ParserCombinators.Parsec.Pos


newParser = do openParenParser 
               str <- syntacticKeywordParser 
               closeParenParser
               return str

main = do
  s <- getContents
  let tokens = alexScanTokens s
  print $ parse newParser "" tokens
