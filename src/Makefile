# li - The Scheme interpreter, powered by Haskell

all : Main.hs Eval/Eval.hs Parser/SParser.hs Lexer/Lexer.hs
	ghc --make Main.hs

clean :
	rm -f *.hi *.o Eval/*.hi Eval/*.o Parser/*.o Parser/*.hi Lexer/*.o Lexer/*.hi

