structure AST = 
struct 

	val nume = ref 0

	fun up () = 
		nume := (!nume) + 1

	type id = string 

	val broken = Fail "Error: Not a valid Id!"

    datatype binop = Add | Sub | Mul | Negate | Lessthan | Greaterthan | And | Not | Or | Xor | Implies | Equals 

    datatype condop = If | Else | Then | Fi 

    datatype stmt = Statement of exp 

    and decl = ValDecl of id * exp 

	and typ =  INT 
	 	     | BOOL 
	 	     | Arrow of typ * typ 

    and exp = Const of bool
    		| NumExp of int 
    	    | VarExp of id 
    	    | BinExp of binop * exp * exp 
    	    | LetExp of decl * exp 
    	    | UnExp of binop * exp 
    	    | IfExp of condop * exp * condop * exp * condop * exp * condop
    	    | AppExp of exp * exp 
    	    | Fn of id * typ * typ * exp 
    	    | Fun of id * id * typ * typ * exp

    and value = IntVal of int 
			   | BoolVal of bool
			   | FunVal of id * typ * typ * exp


    type prgm = (stmt) list 

    type environment = (id * value) list 

	type funEnvironment = (id * value) list

	fun toId (e) = 
		case e of 
			VarExp i => i
		| 	_		 => raise Fail "Not a valid VarExp!"

	fun envAdd (var: id, v: value, env: environment) = 
		(var, v):: env

  	fun envLookUp (var: exp, env: environment, funEnv: funEnvironment) = 
		case List.find (fn (x, _) => x = toId(var)) env of 
			SOME (x,v) => v
		|	NONE => case List.find (fn (x, _) => x = toId (var)) funEnv of
						SOME (x, v) => v
					| 	NONE 	    => raise Fail "Function Environment lookup error!"

    fun funAdd (var:id, v: value, funEnv: funEnvironment) = 
    	
		(var, v)::funEnv
		

	(*fun funLookUp (var: exp, funEnv: funEnvironment) = 

		case List.find (fn (x, _) => x = toId (var)) funEnv of 
			SOME (x, v) => v
		| 	NONE 	    => raise Fail "Function Environment lookup error!"*)

end