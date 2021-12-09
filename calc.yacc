%%

%name Calc 

%term 
	
	ID of string | NUM of int | PLUS   | TIMES    |
	MINUS 		 | NEGATE 	  | EQUALS | LESSTHAN |
	GREATERTHAN  | LET 		  | IN     | END      | 
	VAL			 | EQ      	  | IF	   | ELSE	  |
	THEN		 | FI		  | NOT	   | XOR  	  | 
	OR			 | AND		  | IMPLIES| TERM     |
	LPAREN 		 | RPAREN     | TRUE   | FALSE    | 
	FUN 		 | FN 		  | ARROW  | DARROW   |
	COLON		 | INT 		  | BOOL   | EOF 

%nonterm 
	
	PRGM of AST.prgm  | START of AST.prgm | EXP of AST.exp  | 
	STMTS of AST.prgm | STMT  of AST.stmt | DECL of AST.decl|
	TYPES of AST.typ  

%pos int 

%eop EOF 
%noshift EOF 

%nonassoc DARROW
%left EQ
%left PLUS MINUS 
%left TIMES 
%left NEGATE 
%left EQUALS LESSTHAN GREATERTHAN
%right IMPLIES
%left AND OR XOR
%right NOT
%right ARROW

%start START

%verbose 

%%
	START 	: PRGM  (PRGM)

	PRGM  	: STMTS (STMTS)
		  	| STMT  ([STMT])

   	STMTS 	: EXP TERM PRGM (AST.Statement (EXP)::PRGM)

   	TYPES	: INT 				(AST.INT)
   			| BOOL				(AST.BOOL)
   			| TYPES ARROW TYPES (AST.Arrow (TYPES1, TYPES2))
   			| LPAREN TYPES RPAREN (TYPES)

   	STMT  	: EXP 				(AST.Statement (EXP))

   	DECL 	: VAL ID EQ EXP 	(AST.ValDecl (ID, EXP))

   	EXP 	: NUM 						(AST.NumExp (NUM))
   			| ID 				        (AST.VarExp (ID))
   			| TRUE 					(AST.Const (true))
   			| FALSE					(AST.Const (false))
   			| LPAREN EXP RPAREN 		        (EXP)
   			| EXP PLUS EXP 				(AST.BinExp (AST.Add, EXP1, EXP2))
			| EXP MINUS EXP 			(AST.BinExp (AST.Sub, EXP1, EXP2))	 
			| EXP TIMES EXP 			(AST.BinExp (AST.Mul, EXP1, EXP2)) 
			| EXP AND EXP 				(AST.BinExp (AST.And , EXP1, EXP2))
    		        | EXP OR EXP 				(AST.BinExp (AST.Or , EXP1, EXP2))
    		        | EXP XOR EXP 				(AST.BinExp (AST.Xor , EXP1, EXP2))
    		        | EXP IMPLIES EXP 			(AST.BinExp (AST.Implies , EXP1, EXP2))
    		        | EXP EQUALS EXP 			(AST.BinExp (AST.Equals , EXP1, EXP2))
    		        | EXP GREATERTHAN EXP 		        (AST.BinExp (AST.Greaterthan, EXP1, EXP2))
			| EXP LESSTHAN EXP 			(AST.BinExp (AST.Lessthan, EXP1, EXP2))
			| NEGATE EXP 				(AST.UnExp  (AST.Negate, EXP))
			| NOT EXP 			        (AST.UnExp  (AST.Not, EXP))
			| LET DECL IN EXP END 		        (AST.LetExp (DECL, EXP))
			| FN LPAREN ID COLON TYPES RPAREN COLON TYPES DARROW EXP 	 (AST.Fn (ID, TYPES1, TYPES2, EXP))
			| FUN ID LPAREN ID COLON TYPES RPAREN COLON TYPES DARROW EXP (AST.Fun (ID1, ID2, TYPES1, TYPES2, EXP))
			| IF EXP THEN EXP ELSE EXP FI        						 (AST.IfExp (AST.If, EXP1, AST.Then, EXP2, AST.Else, EXP3, AST.Fi))
			| LPAREN EXP EXP RPAREN     (AST.AppExp (EXP1, EXP2))
