
all:
	mllex ass.lex
	mlyacc ass.yacc
	mlton a2.mlb
	./a2