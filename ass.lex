structure Tokens = Tokens 
	
	type pos = int 
	type svalue = Tokens.svalue 
	type ('a, 'b) token = ('a, 'b) Tokens.token 
	type lexresult = (svalue, pos) token 
	val pos = ref 1
	val col = ref 1
	fun refinc x = (x := !x +1; !x)
	val eof = fn () => Tokens.EOF (!pos, !pos)
	val error = fn (token, col:int, line:int) => TextIO.output (TextIO.stdOut,"Unknown Token:" ^ (Int.toString line) ^ ":"^ (Int.toString col) ^ ":" ^ token ^ "\n")
	
%%
%header (functor BoolLexFun (structure Tokens:Bool_TOKENS)) ;
alpha = [A-Za-z];
ws = [\ \t];
%%

\n 			=> (pos := (!pos) + 1; col := (0); lex());
{ws}+ 		=> (col := (!col) + 1 ;lex());
"AND" 		=> (print ("AND " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 3 ; Tokens.AND(yytext, !pos, !pos));
"OR" 		=> (print ("OR " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 2 ; Tokens.OR(yytext, !pos, !pos));
"XOR" 		=> (print ("XOR " ^ "\"" ^ yytext ^ "\" "); col := (!col) + 3 ; Tokens.XOR(yytext, !pos, !pos));
"EQUALS" 	=> (print ("EQUALS " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 6 ; Tokens.EQUALS(yytext, !pos, !pos));
"NOT" 		=> (print ("NOT " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 7 ; Tokens.NOT(yytext, !pos, !pos));
"IMPLIES" 	=> (print ("IMPLIES " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 3 ; Tokens.IMPLIES(yytext, !pos, !pos));
";" 		=> (print ("TERM " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 1 ; Tokens.TERM(yytext, !pos, !pos));
"TRUE" 		=> (print ("TRUE " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 4 ; Tokens.CONST(yytext, !pos,!pos));
"FALSE"		=> (print ("FALSE " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 5 ; Tokens.CONST(yytext, !pos, !pos));
"(" 		=> (print ("LPAREN " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 1 ; Tokens.LPAREN(yytext, !pos, !pos));
")" 		=> (print ("RPAREN " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 1 ; Tokens.RPAREN(yytext, !pos, !pos));
"IF" 		=> (print ("IF " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 2 ; Tokens.IF(yytext, !pos, !pos));
"ELSE" 		=> (print ("ELSE " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 4 ; Tokens.ELSE(yytext, !pos, !pos));
"THEN" 		=> (print ("THEN " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 4 ; Tokens.THEN(yytext, !pos, !pos));
{alpha}+ 	=> (print ("ID " ^ "\"" ^ yytext ^ "\", "); col := (!col) + String.size (yytext) ; Tokens.ID(yytext, !pos,!pos));
. 			=> (error (yytext, !col, !pos);lex());