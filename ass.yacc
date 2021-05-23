(* user declarations *)
%%
(*required declaration*)
%name Bool

%term 
    IF of string | ELSE of string | THEN of string | TERM of string | ID of string | IMPLIES of string | EQUALS of string | OR of string | XOR of string | AND of string | NOT of string | CONST of string | LPAREN of string | RPAREN of string | EOF 

%nonterm PRGM of string | STMT of string| FML of string | START of string 

%pos int 
%eop EOF
%noshift EOF 

%right IF THEN ELSE 
%right IMPLIES
%left EQUALS AND OR XOR
%right NOT

%start START 
%verbose 

%%
START: PRGM (PRGM ^ "program\n")
PRGM: STMT (STMT)

STMT : FML TERM PRGM (FML ^ " formula, " ^   "\";\" " ^ "TERM, " ^ "statement, " ^ PRGM)
        | FML TERM (FML ^ " formula, "  ^ "\";\" " ^ "TERM, " ^ "statement, ")

FML: LPAREN FML RPAREN ("\"(\"" ^ " LPAREN, " ^ FML ^ " formula, " ^ "\")\"" ^ "RPAREN, ")
    |IF FML THEN FML ELSE FML ("\"IF\" " ^ "IF, " ^ FML1 ^ " formula, " ^ "\"THEN\" " ^ "THEN, " ^ FML2 ^ " formula, " ^ "\"ELSE\" " ^ " ELSE, " ^ FML3 ^ " formula, " )
    |FML IMPLIES FML (FML1 ^ " formula, " ^ "\"IMPLIES\" " ^ " IMPLIES, "  ^ FML2 ^ " formula, ")
    |FML EQUALS FML (FML1 ^ " formula, " ^ "\"EQUALS\" " ^ " EQUALS, "  ^ FML2 ^ " formula, " )
    |FML XOR FML (FML1 ^ " formula, " ^ "\"XOR\" " ^ " XOR, " ^ FML2 ^ " formula, " )
    |FML OR FML  (FML1 ^ " formula, " ^ "\"OR\" " ^ " OR, " ^ FML2 ^ " formula, " )
    |FML AND FML (FML1 ^  " formula, " ^ "\"AND\" " ^ " AND, " ^ FML2 ^ " formula, ")
    |NOT FML  ("\"NOT\" " ^ " NOT, " ^ FML ^ " formula, ")
    |CONST ("\"" ^ CONST ^ "\"" ^ " CONST, " )
    |ID ("\"" ^ ID ^ "\"" ^ " ID, ")



