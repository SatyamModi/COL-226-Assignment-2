functor CalcLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Calc_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(*#line 1.2 "calc.yacc"*)
(*#line 12.1 "calc.yacc.sml"*)
end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\017\000\002\000\016\000\003\000\028\000\004\000\027\000\
\\005\000\026\000\006\000\015\000\007\000\025\000\008\000\024\000\
\\009\000\023\000\010\000\014\000\015\000\013\000\019\000\012\000\
\\020\000\022\000\021\000\021\000\022\000\020\000\023\000\019\000\
\\025\000\011\000\026\000\051\000\027\000\010\000\028\000\009\000\
\\029\000\008\000\030\000\007\000\000\000\
\\001\000\001\000\017\000\002\000\016\000\006\000\015\000\010\000\014\000\
\\015\000\013\000\019\000\012\000\025\000\011\000\027\000\010\000\
\\028\000\009\000\029\000\008\000\030\000\007\000\000\000\
\\001\000\001\000\030\000\000\000\
\\001\000\001\000\048\000\000\000\
\\001\000\001\000\054\000\000\000\
\\001\000\001\000\056\000\000\000\
\\001\000\003\000\028\000\004\000\027\000\005\000\026\000\007\000\025\000\
\\008\000\024\000\009\000\023\000\012\000\066\000\020\000\022\000\
\\021\000\021\000\022\000\020\000\023\000\019\000\000\000\
\\001\000\003\000\028\000\004\000\027\000\005\000\026\000\007\000\025\000\
\\008\000\024\000\009\000\023\000\016\000\065\000\020\000\022\000\
\\021\000\021\000\022\000\020\000\023\000\019\000\000\000\
\\001\000\003\000\028\000\004\000\027\000\005\000\026\000\007\000\025\000\
\\008\000\024\000\009\000\023\000\017\000\052\000\020\000\022\000\
\\021\000\021\000\022\000\020\000\023\000\019\000\000\000\
\\001\000\003\000\028\000\004\000\027\000\005\000\026\000\007\000\025\000\
\\008\000\024\000\009\000\023\000\018\000\075\000\020\000\022\000\
\\021\000\021\000\022\000\020\000\023\000\019\000\000\000\
\\001\000\003\000\028\000\004\000\027\000\005\000\026\000\007\000\025\000\
\\008\000\024\000\009\000\023\000\020\000\022\000\021\000\021\000\
\\022\000\020\000\023\000\019\000\026\000\057\000\000\000\
\\001\000\011\000\053\000\000\000\
\\001\000\013\000\035\000\000\000\
\\001\000\014\000\060\000\000\000\
\\001\000\025\000\029\000\000\000\
\\001\000\025\000\049\000\000\000\
\\001\000\026\000\069\000\031\000\068\000\000\000\
\\001\000\026\000\074\000\031\000\068\000\000\000\
\\001\000\031\000\068\000\032\000\078\000\000\000\
\\001\000\031\000\068\000\032\000\081\000\000\000\
\\001\000\033\000\055\000\000\000\
\\001\000\033\000\064\000\000\000\
\\001\000\033\000\073\000\000\000\
\\001\000\033\000\077\000\000\000\
\\001\000\034\000\063\000\035\000\062\000\000\000\
\\001\000\036\000\000\000\000\000\
\\084\000\000\000\
\\085\000\000\000\
\\086\000\000\000\
\\087\000\000\000\
\\088\000\000\000\
\\089\000\000\000\
\\090\000\031\000\068\000\000\000\
\\091\000\003\000\028\000\004\000\027\000\005\000\026\000\007\000\025\000\
\\008\000\024\000\009\000\023\000\020\000\022\000\021\000\021\000\
\\022\000\020\000\023\000\019\000\024\000\018\000\000\000\
\\092\000\003\000\028\000\004\000\027\000\005\000\026\000\007\000\025\000\
\\008\000\024\000\009\000\023\000\020\000\022\000\021\000\021\000\
\\022\000\020\000\023\000\019\000\000\000\
\\093\000\000\000\
\\094\000\000\000\
\\095\000\000\000\
\\096\000\000\000\
\\097\000\000\000\
\\098\000\004\000\027\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\020\000\022\000\021\000\021\000\022\000\020\000\023\000\019\000\000\000\
\\099\000\004\000\027\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\020\000\022\000\021\000\021\000\022\000\020\000\023\000\019\000\000\000\
\\100\000\007\000\025\000\008\000\024\000\009\000\023\000\020\000\022\000\
\\021\000\021\000\022\000\020\000\023\000\019\000\000\000\
\\101\000\000\000\
\\102\000\000\000\
\\103\000\000\000\
\\104\000\020\000\022\000\021\000\021\000\022\000\020\000\023\000\019\000\000\000\
\\105\000\020\000\022\000\021\000\021\000\022\000\020\000\023\000\019\000\000\000\
\\106\000\020\000\022\000\021\000\021\000\022\000\020\000\023\000\019\000\000\000\
\\107\000\020\000\022\000\021\000\021\000\022\000\020\000\023\000\019\000\000\000\
\\108\000\007\000\025\000\008\000\024\000\009\000\023\000\020\000\022\000\
\\021\000\021\000\022\000\020\000\023\000\019\000\000\000\
\\109\000\000\000\
\\110\000\000\000\
\\111\000\003\000\028\000\004\000\027\000\005\000\026\000\007\000\025\000\
\\008\000\024\000\009\000\023\000\020\000\022\000\021\000\021\000\
\\022\000\020\000\023\000\019\000\000\000\
\\112\000\003\000\028\000\004\000\027\000\005\000\026\000\007\000\025\000\
\\008\000\024\000\009\000\023\000\020\000\022\000\021\000\021\000\
\\022\000\020\000\023\000\019\000\000\000\
\\113\000\000\000\
\\114\000\000\000\
\"
val actionRowNumbers =
"\001\000\028\000\027\000\033\000\
\\026\000\014\000\002\000\038\000\
\\037\000\001\000\001\000\001\000\
\\012\000\001\000\035\000\036\000\
\\001\000\001\000\001\000\001\000\
\\001\000\001\000\001\000\001\000\
\\001\000\001\000\001\000\003\000\
\\015\000\000\000\051\000\008\000\
\\011\000\004\000\050\000\029\000\
\\046\000\043\000\044\000\045\000\
\\048\000\049\000\047\000\041\000\
\\042\000\040\000\020\000\005\000\
\\010\000\039\000\001\000\001\000\
\\013\000\024\000\021\000\056\000\
\\007\000\006\000\001\000\016\000\
\\031\000\030\000\024\000\001\000\
\\052\000\034\000\024\000\022\000\
\\017\000\009\000\032\000\024\000\
\\023\000\055\000\018\000\024\000\
\\001\000\019\000\053\000\001\000\
\\054\000\025\000"
val gotoT =
"\
\\001\000\004\000\002\000\081\000\003\000\003\000\004\000\002\000\
\\005\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\029\000\000\000\
\\003\000\030\000\000\000\
\\003\000\031\000\000\000\
\\006\000\032\000\000\000\
\\003\000\034\000\000\000\
\\000\000\
\\000\000\
\\001\000\035\000\003\000\003\000\004\000\002\000\005\000\001\000\000\000\
\\003\000\036\000\000\000\
\\003\000\037\000\000\000\
\\003\000\038\000\000\000\
\\003\000\039\000\000\000\
\\003\000\040\000\000\000\
\\003\000\041\000\000\000\
\\003\000\042\000\000\000\
\\003\000\043\000\000\000\
\\003\000\044\000\000\000\
\\003\000\045\000\000\000\
\\000\000\
\\000\000\
\\003\000\048\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\056\000\000\000\
\\003\000\057\000\000\000\
\\000\000\
\\007\000\059\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\065\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\068\000\000\000\
\\003\000\069\000\000\000\
\\000\000\
\\000\000\
\\007\000\070\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\074\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\077\000\000\000\
\\003\000\078\000\000\000\
\\000\000\
\\000\000\
\\003\000\080\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 82
val numrules = 31
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit | NUM of unit ->  (int) | ID of unit ->  (string) | TYPES of unit ->  (AST.typ) | DECL of unit ->  (AST.decl) | STMT of unit ->  (AST.stmt) | STMTS of unit ->  (AST.prgm) | EXP of unit ->  (AST.exp) | START of unit ->  (AST.prgm) | PRGM of unit ->  (AST.prgm)
end
type svalue = MlyValue.svalue
type result = AST.prgm
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 35) => true | _ => false
val showTerminal =
fn (T 0) => "ID"
  | (T 1) => "NUM"
  | (T 2) => "PLUS"
  | (T 3) => "TIMES"
  | (T 4) => "MINUS"
  | (T 5) => "NEGATE"
  | (T 6) => "EQUALS"
  | (T 7) => "LESSTHAN"
  | (T 8) => "GREATERTHAN"
  | (T 9) => "LET"
  | (T 10) => "IN"
  | (T 11) => "END"
  | (T 12) => "VAL"
  | (T 13) => "EQ"
  | (T 14) => "IF"
  | (T 15) => "ELSE"
  | (T 16) => "THEN"
  | (T 17) => "FI"
  | (T 18) => "NOT"
  | (T 19) => "XOR"
  | (T 20) => "OR"
  | (T 21) => "AND"
  | (T 22) => "IMPLIES"
  | (T 23) => "TERM"
  | (T 24) => "LPAREN"
  | (T 25) => "RPAREN"
  | (T 26) => "TRUE"
  | (T 27) => "FALSE"
  | (T 28) => "FUN"
  | (T 29) => "FN"
  | (T 30) => "ARROW"
  | (T 31) => "DARROW"
  | (T 32) => "COLON"
  | (T 33) => "INT"
  | (T 34) => "BOOL"
  | (T 35) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.PRGM PRGM1, PRGM1left, PRGM1right)) :: rest671)) => let val  result = MlyValue.START (fn _ => let val  (PRGM as PRGM1) = PRGM1 ()
 in ((*#line 44.18 "calc.yacc"*)PRGM(*#line 345.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 1, ( result, PRGM1left, PRGM1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.STMTS STMTS1, STMTS1left, STMTS1right)) :: rest671)) => let val  result = MlyValue.PRGM (fn _ => let val  (STMTS as STMTS1) = STMTS1 ()
 in ((*#line 46.18 "calc.yacc"*)STMTS(*#line 351.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, STMTS1left, STMTS1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.STMT STMT1, STMT1left, STMT1right)) :: rest671)) => let val  result = MlyValue.PRGM (fn _ => let val  (STMT as STMT1) = STMT1 ()
 in ((*#line 47.15 "calc.yacc"*)[STMT](*#line 357.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, STMT1left, STMT1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.PRGM PRGM1, _, PRGM1right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.STMTS (fn _ => let val  (EXP as EXP1) = EXP1 ()
 val  (PRGM as PRGM1) = PRGM1 ()
 in ((*#line 49.29 "calc.yacc"*)AST.Statement (EXP)::PRGM(*#line 363.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 3, ( result, EXP1left, PRGM1right), rest671)
end
|  ( 4, ( ( _, ( _, INT1left, INT1right)) :: rest671)) => let val  result = MlyValue.TYPES (fn _ => ((*#line 51.22 "calc.yacc"*)AST.INT(*#line 370.1 "calc.yacc.sml"*)
))
 in ( LrTable.NT 6, ( result, INT1left, INT1right), rest671)
end
|  ( 5, ( ( _, ( _, BOOL1left, BOOL1right)) :: rest671)) => let val  result = MlyValue.TYPES (fn _ => ((*#line 52.18 "calc.yacc"*)AST.BOOL(*#line 374.1 "calc.yacc.sml"*)
))
 in ( LrTable.NT 6, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.TYPES TYPES2, _, TYPES2right)) :: _ :: ( _, ( MlyValue.TYPES TYPES1, TYPES1left, _)) :: rest671)) => let val  result = MlyValue.TYPES (fn _ => let val  TYPES1 = TYPES1 ()
 val  TYPES2 = TYPES2 ()
 in ((*#line 53.28 "calc.yacc"*)AST.Arrow (TYPES1, TYPES2)(*#line 378.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 6, ( result, TYPES1left, TYPES2right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.EXP EXP1, EXP1left, EXP1right)) :: rest671)) => let val  result = MlyValue.STMT (fn _ => let val  (EXP as EXP1) = EXP1 ()
 in ((*#line 55.23 "calc.yacc"*)AST.Statement (EXP)(*#line 385.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 4, ( result, EXP1left, EXP1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, VAL1left, _)) :: rest671)) => let val  result = MlyValue.DECL (fn _ => let val  (ID as ID1) = ID1 ()
 val  (EXP as EXP1) = EXP1 ()
 in ((*#line 57.29 "calc.yacc"*)AST.ValDecl (ID, EXP)(*#line 391.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 5, ( result, VAL1left, EXP1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (NUM as NUM1) = NUM1 ()
 in ((*#line 59.23 "calc.yacc"*)AST.NumExp (NUM)(*#line 398.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, NUM1left, NUM1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (ID as ID1) = ID1 ()
 in ((*#line 60.19 "calc.yacc"*)AST.VarExp (ID)(*#line 404.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, ID1left, ID1right), rest671)
end
|  ( 11, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => ((*#line 61.21 "calc.yacc"*)AST.Const (true)(*#line 410.1 "calc.yacc.sml"*)
))
 in ( LrTable.NT 2, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 12, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => ((*#line 62.21 "calc.yacc"*)AST.Const (false)(*#line 414.1 "calc.yacc.sml"*)
))
 in ( LrTable.NT 2, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 13, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 in ((*#line 63.30 "calc.yacc"*)EXP(*#line 418.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 64.27 "calc.yacc"*)AST.BinExp (AST.Add, EXP1, EXP2)(*#line 424.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 65.24 "calc.yacc"*)AST.BinExp (AST.Sub, EXP1, EXP2)(*#line 431.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 66.24 "calc.yacc"*)AST.BinExp (AST.Mul, EXP1, EXP2)(*#line 438.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 67.23 "calc.yacc"*)AST.BinExp (AST.And , EXP1, EXP2)(*#line 445.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 68.25 "calc.yacc"*)AST.BinExp (AST.Or , EXP1, EXP2)(*#line 452.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 69.26 "calc.yacc"*)AST.BinExp (AST.Xor , EXP1, EXP2)(*#line 459.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 70.29 "calc.yacc"*)AST.BinExp (AST.Implies , EXP1, EXP2)(*#line 466.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 71.28 "calc.yacc"*)AST.BinExp (AST.Equals , EXP1, EXP2)(*#line 473.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 72.32 "calc.yacc"*)AST.BinExp (AST.Greaterthan, EXP1, EXP2)(*#line 480.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 73.27 "calc.yacc"*)AST.BinExp (AST.Lessthan, EXP1, EXP2)(*#line 487.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: ( _, ( _, NEGATE1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 in ((*#line 74.22 "calc.yacc"*)AST.UnExp  (AST.Negate, EXP)(*#line 494.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, NEGATE1left, EXP1right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: ( _, ( _, NOT1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 in ((*#line 75.20 "calc.yacc"*)AST.UnExp  (AST.Not, EXP)(*#line 500.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, NOT1left, EXP1right), rest671)
end
|  ( 26, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.EXP EXP1, _, _)) :: _ :: ( _, ( MlyValue.DECL DECL1, _, _)) :: ( _, ( _, LET1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (DECL as DECL1) = DECL1 ()
 val  (EXP as EXP1) = EXP1 ()
 in ((*#line 76.29 "calc.yacc"*)AST.LetExp (DECL, EXP)(*#line 506.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, LET1left, END1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: _ :: ( _, ( MlyValue.TYPES TYPES2, _, _)) :: _ :: _ :: ( _, ( MlyValue.TYPES TYPES1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: _ :: ( _, ( _, FN1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (ID as ID1) = ID1 ()
 val  TYPES1 = TYPES1 ()
 val  TYPES2 = TYPES2 ()
 val  (EXP as EXP1) = EXP1 ()
 in ((*#line 77.64 "calc.yacc"*)AST.Fn (ID, TYPES1, TYPES2, EXP)(*#line 513.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, FN1left, EXP1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: _ :: ( _, ( MlyValue.TYPES TYPES2, _, _)) :: _ :: _ :: ( _, ( MlyValue.TYPES TYPES1, _, _)) :: _ :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, FUN1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  TYPES1 = TYPES1 ()
 val  TYPES2 = TYPES2 ()
 val  (EXP as EXP1) = EXP1 ()
 in ((*#line 78.66 "calc.yacc"*)AST.Fun (ID1, ID2, TYPES1, TYPES2, EXP)(*#line 522.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, FUN1left, EXP1right), rest671)
end
|  ( 29, ( ( _, ( _, _, FI1right)) :: ( _, ( MlyValue.EXP EXP3, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP2, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 val  EXP3 = EXP3 ()
 in ((*#line 79.49 "calc.yacc"*)AST.IfExp (AST.If, EXP1, AST.Then, EXP2, AST.Else, EXP3, AST.Fi)(*#line 532.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, IF1left, FI1right), rest671)
end
|  ( 30, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXP EXP2, _, _)) :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 80.33 "calc.yacc"*)AST.AppExp (EXP1, EXP2)(*#line 540.1 "calc.yacc.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, LPAREN1left, RPAREN1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.START x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Calc_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(ParserData.MlyValue.ID (fn () => i),p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(ParserData.MlyValue.NUM (fn () => i),p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(ParserData.MlyValue.VOID,p1,p2))
fun NEGATE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(ParserData.MlyValue.VOID,p1,p2))
fun EQUALS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(ParserData.MlyValue.VOID,p1,p2))
fun LESSTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(ParserData.MlyValue.VOID,p1,p2))
fun GREATERTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(ParserData.MlyValue.VOID,p1,p2))
fun VAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(ParserData.MlyValue.VOID,p1,p2))
fun FI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(ParserData.MlyValue.VOID,p1,p2))
fun XOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(ParserData.MlyValue.VOID,p1,p2))
fun IMPLIES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(ParserData.MlyValue.VOID,p1,p2))
fun TERM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(ParserData.MlyValue.VOID,p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(ParserData.MlyValue.VOID,p1,p2))
fun FUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(ParserData.MlyValue.VOID,p1,p2))
fun FN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(ParserData.MlyValue.VOID,p1,p2))
fun ARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(ParserData.MlyValue.VOID,p1,p2))
fun DARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(ParserData.MlyValue.VOID,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(ParserData.MlyValue.VOID,p1,p2))
end
end
