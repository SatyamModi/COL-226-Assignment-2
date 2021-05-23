structure Tokens = Tokens 
	
	type pos = int 
	type svalue = Tokens.svalue 
	type ('a, 'b) token = ('a, 'b) Tokens.token 
	type lexresult = (svalue, pos) token 

	val pos = ref 0
	val eof = fn () => Tokens.EOF (!pos, !pos)
	val error = fn (e, l:int, _) => TextIO.output (TextIO.stdOut, "line " ^ (Int.toString l) ^ ": " ^ e ^ "\n")

	val keywords = 
	[
		("end", Tokens.END), 
		("in", Tokens.IN),
		("let", Tokens.LET),
		("val", Tokens.VAL), 
		("if", Tokens.IF), 
		("else", Tokens.ELSE), 
		("then", Tokens.THEN),
		("fi", Tokens.FI),
		("fn", Tokens.FN),
		("fun", Tokens.FUN),
		("int", Tokens.INT),
		("bool", Tokens.BOOL)
	]

	fun findKeywords (str:string, pos1: pos, pos2: pos) = 
		case List.find (fn (s, _) => s = str) keywords of 
			SOME (_, tk) => tk(pos1, pos2)
			| NONE => Tokens.ID (str, pos1, pos2)

%%

%header (functor CalcLexFun (structure Tokens: Calc_TOKENS));

alpha = [A-Za-z];
digit = [0-9];
alphanum = [A-Za-z0-9];
ws = [\ \t];

%%
\n				=> (pos := (!pos) + 1; lex());
{ws}+			=> (lex());
{digit}+		=> (Tokens.NUM (valOf (Int.fromString yytext), !pos, !pos));
"PLUS"			=> (Tokens.PLUS (!pos, !pos));
"TIMES"			=> (Tokens.TIMES (!pos, !pos));
"NEGATE"		=> (Tokens.NEGATE (!pos, !pos));
"MINUS"			=> (Tokens.MINUS (!pos, !pos));
"EQUALS"		=> (Tokens.EQUALS (!pos, !pos));
"GREATERTHAN" 	=> (Tokens.GREATERTHAN (!pos, !pos));
"LESSTHAN"		=> (Tokens.LESSTHAN (!pos, !pos));
"NOT"			=> (Tokens.NOT (!pos, !pos));
"AND"			=> (Tokens.AND (!pos, !pos));
"XOR"			=> (Tokens.XOR (!pos, !pos));
"OR"			=> (Tokens.OR (!pos, !pos));
"IMPLIES"		=> (Tokens.IMPLIES (!pos, !pos));
"TRUE"			=> (Tokens.TRUE (!pos, !pos));
"FALSE"			=> (Tokens.FALSE (!pos, !pos));
";"				=> (Tokens.TERM (!pos, !pos));
"("				=> (Tokens.LPAREN (!pos, !pos));
")"				=> (Tokens.RPAREN (!pos, !pos));
{alpha}{alphanum}*	=> (findKeywords(yytext, !pos, !pos));
"="				=> (Tokens.EQ (!pos, !pos));
":"				=> (Tokens.COLON (!pos, !pos));
"=>"			=> (Tokens.DARROW (!pos, !pos));
"->"			=> (Tokens.ARROW (!pos, !pos));
"."				=> (error ("ignoring bad character " ^ yytext, !pos, !pos); lex());