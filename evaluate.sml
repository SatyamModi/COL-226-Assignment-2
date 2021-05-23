structure EVALUATOR = 
struct 
open AST 

val brokenTypes = Fail "Error in evaluation"

val num = ref 0

val funEnv = []

fun update () = 
	num := (!num) + 1

fun toExp (stm) =
	
	let 
		val x = update ()
	in 
		case stm of 
			Statement e => e
	end 

fun getType (v) = 

	case v of 
		IntVal _	=> INT 
	|	BoolVal _	=> BOOL
	|   FunVal (b, t1, t2, e) => Arrow (t1, t2)

fun tcheckInt (v) = 
    case v of 
        IntVal i 	=> true
    |   _ 			=> false

fun tcheckBool (v) = 
    case v of 
        BoolVal i 	=> true
    |   _ 			=> false

fun evalPrgm (prg) = 
	let
		val env = []
		val idx = 0
		val res = []
	in
		evalStmt (idx, prg, env, funEnv, res)
	end

and evalStmt (idx, prg, env, funEnv, res) =

	if (idx = List.length (prg)) then 
		res 
	else 
		let 
			val Statement e = List.nth (prg, idx)
		in 
			case e of 
				Const y		=> evalStmt (idx + 1, prg, env, funEnv, res @ [evalExp (e, env, funEnv)])
			| 	NumExp i 	=> evalStmt (idx + 1, prg, env, funEnv, res @ [evalExp (e, env, funEnv)])
			|	VarExp x 	=> evalStmt (idx + 1, prg, env, funEnv, res @ [evalExp (e, env, funEnv)])
			|	UnExp (unop, e1) => evalStmt (idx + 1, prg, env, funEnv, res @ [evalExp (e, env, funEnv)])
			|	BinExp (b, e1, e2) => evalStmt (idx + 1, prg, env, funEnv, res @ [evalExp (e, env, funEnv)])
			|	IfExp  (_, e1, _, e2, _, e3, _) =>  evalStmt (idx + 1, prg, env, funEnv, res @ [evalExp (e, env, funEnv)])
			|	LetExp (ValDecl (x, e1), e2)	=>  evalStmt (idx + 1, prg, env, funEnv, res @ [evalExp (e, env, funEnv)])
			|	Fn (b, t1, t2, e)				=>  evalStmt (idx + 1, prg, env, funEnv, res @ [evalExp (e, env, funEnv)])
			|	AppExp (e1, e2)					=>  evalStmt (idx + 1, prg, env, funEnv, res @ [evalExp (e, env, funEnv)])
			|	Fun (name, b, t1, t2, e1)		=>  let 
														val z = evalExp (e, env, funEnv)
														val funEnv2 = funAdd (name, z, funEnv)
													in 
														evalStmt (idx + 1, prg, env, funEnv2, res @ [z])
													end
		end


and evalExp (e: exp, env: environment, funEnv) =

	case e of 
		Const y			=> BoolVal y
	|	NumExp i    	=> IntVal i
	|	VarExp x		=> envLookUp (e, env, funEnv)
	| 	UnExp (unop, e1) => if unop = Negate then 
								let 
									val v = evalExp (e1, env, funEnv)
								in 
									case v of 
										IntVal i 	=> IntVal (~1 * i)
									|	_			=> raise Fail "Not a Int type expression!"
								end 

							else 			
								let 
									val v = evalExp (e1, env, funEnv)
								in 	
									case v of 
										BoolVal i 	=> 	if i = true then 
															BoolVal false
														else 
															BoolVal true 
									|	_			=> 	raise Fail "Not a Bool type expression!"
								end 


	|	BinExp (b, e1, e2) => evalBinExp (b, e1, e2, env, funEnv)

	| 	IfExp  (_, e1, _, e2, _, e3, _)	=> 	if  evalExp (e1, env, funEnv) = BoolVal true then 
                	  							evalExp (e2, env, funEnv) 
                  							else 
                  								evalExp (e3, env, funEnv)
          								

	|	LetExp (ValDecl (x, e1), e2)	=>  let 
												val v1 = evalExp (e1, env, funEnv) 
											in 
												evalExp (e2, envAdd (x, v1, env), funEnv)
											end 
	
	| 	Fn (b, t1, t2, e)				=> 	FunVal (b, t1, t2, e)


	|	AppExp (e1, e2)					=> 	let
												val FunVal f = envLookUp (e1, env, funEnv)
												val b = #1(f)
												val t1 = #2(f)
												val t2 = #3(f)
												val e3 = #4(f)
												val v = evalExp (e2, env, funEnv)
											in 
												if t1 = getType (v) then
													let 
														val v1 = evalExp (e3, envAdd (b, v, env), funEnv)
													in
														if t2 = getType(v1) then 
															v1
														else 
															raise Fail "Output value don't match the return type of the function!"
													end 
												else 
													raise Fail "Input value type don't match the input type of the function!"

											end 

											
	| 	Fun (name, b, t1, t2, e1)		=>  FunVal (b, t1, t2, e1)
											

and evalBinExp (b: binop, e1: exp, e2: exp, env: environment, funEnv) = 

		case (b, evalExp (e1, env, funEnv), evalExp (e2, env, funEnv)) of 

			(Add, IntVal i1, IntVal i2) 	=> IntVal (i1+i2)

	  	| 	(Sub, IntVal i1, IntVal i2) 	=> IntVal (i1-i2)

	  	| 	(Mul, IntVal i1, IntVal i2) 	=> IntVal (i1*i2)

	  	| 	(Equals, IntVal i1, IntVal i2)  => BoolVal (i1 = i2)

	  	| 	(Greaterthan, IntVal i1, IntVal i2) => BoolVal (i1 > i2)

	  	| 	(Lessthan, IntVal i1, IntVal i2)	=> BoolVal (i1 < i2)

	  	| 	(And, BoolVal i1, BoolVal i2)		=> BoolVal (i1 andalso i2)

	  	|	(Or, BoolVal i1, BoolVal i2)		=> 	if i1 = true then 
          												BoolVal (true) 
        										    else 
          												if i2 = true then 
          													BoolVal (true)
            											else 
            												BoolVal (false)
		| 	(Xor, BoolVal i1, BoolVal i2) 		=> 	if i1 <> i2 then 
														BoolVal true 
													else 
														BoolVal false 

		|	(Implies, BoolVal i1, BoolVal i2)   => if i1 = true andalso i2 = false then 
														BoolVal true 
													else 
														BoolVal false

		| 	(Equals, BoolVal i1, BoolVal i2)    => if i1 = i2 then 
          												BoolVal (true)
        										   else 
        										   	    BoolVal (false)

     	| 	(b, t1, t2) =>  if b = Add then 
		                      	if (tcheckInt (t1)) then 
		                         	if (tcheckInt (t2)) then 
		                             	raise Fail "no problem"
	                         	else 
	                             	raise Fail ("Bool type found in PLUS operation" ^ " in Statement " ^ Int.toString (!num))
	                      	else
	                          	raise Fail ("Bool type found in PLUS operation" ^ " in Statement " ^ Int.toString (!num))

		                    else if b = Sub then
		                      	if (tcheckInt (t1)) then 
		                         	if (tcheckInt (t2)) then 
		                             	raise Fail "no problem"
		                         	else 
		                             	raise Fail ("Bool type found in MINUS operation" ^ " in Statement " ^ Int.toString (!num))
		                      	else
		                          	raise Fail ("Bool type found in MINUS operation" ^ " in Statement " ^ Int.toString (!num))

		                    else if b = Mul then
		                      	if (tcheckInt (t1)) then 
		                         	if (tcheckInt (t2)) then 
		                             	raise Fail "no problem"
		                         	else 
		                             	raise Fail ("Bool type found in TIMES operation" ^ " in Statement " ^ Int.toString (!num))
		                      	else
		                          	raise Fail ("Bool type found in TIMES operation" ^ " in Statement " ^ Int.toString (!num))

		                    else if b = Lessthan then
		                      	if (tcheckInt (t1)) then 
		                         	if (tcheckInt (t2)) then 
		                             	raise Fail "no problem"
		                         	else 
		                             	raise Fail ("Bool type found in LESSTHAN operation" ^ " in Statement " ^ Int.toString (!num))
		                      	else
		                          	raise Fail ("Bool type found in LESSTHAN operation" ^ " in Statement " ^ Int.toString (!num))

		                    else if b = Greaterthan then
		                      	if (tcheckInt (t1)) then 
		                         	if (tcheckInt (t2)) then 
		                             	raise Fail "no problem"
		                         	else 
		                             	raise Fail ("Bool type found in GREATERTHAN operation" ^ " in Statement " ^ Int.toString (!num))
	                     	 	else
		                          	raise Fail ("Bool type found in GREATERTHAN operation" ^ " in Statement " ^ Int.toString (!num))

		                    else if b = And then 
		                      	if (tcheckBool (t1)) then 
		                         	if (tcheckBool (t2)) then 
		                             	raise Fail "no problem"
	                         		else 
		                             	raise Fail ("Int type found in AND operation" ^ " in Statement " ^ Int.toString (!num))
		                      	else
		                          	raise Fail ("Int type found in AND operation" ^ " in Statement " ^ Int.toString (!num))

		                    else if b = Or then 
		                      	if (tcheckBool (t1)) then 
		                         	if (tcheckBool (t2)) then 
		                             	raise Fail "no problem"
		                         	else 
		                             	raise Fail ("Int type found in OR operation" ^ " in Statement " ^ Int.toString (!num))
		                      	else
		                          	raise Fail ("Int type found in OR operation" ^ " in Statement " ^ Int.toString (!num))

		                    else if b = Xor then 
		                      	if (tcheckBool (t1)) then 
		                         	if (tcheckBool (t2)) then 
		                             	raise Fail "no problem"
		                         	else 
		                             	raise Fail ("Int type found in XOR operation" ^ " in Statement " ^ Int.toString (!num))
		                      	else
		                          	raise Fail ("Int type found in XOR operation" ^ " in Statement " ^ Int.toString (!num))

		                    else if b = Implies then 
		                      	if (tcheckBool (t1)) then 
		                         	if (tcheckBool (t2)) then 
		                             	raise Fail "no problem"
		                         	else 
		                             	raise Fail ("Int type found in IMPLIES operation" ^ " in Statement " ^ Int.toString (!num))
		                      	else
		                          	raise Fail ("Int type found in IMPLIES operation" ^ " in Statement " ^ Int.toString (!num))

		                    else if b = Equals then 
		                      	if (tcheckBool (t1)) then 
		                         	if (tcheckBool (t2)) then 
		                             	raise Fail "no problem"
		                         	else 
		                             	raise Fail ("Int type found in Equals operation" ^ " in Statement " ^ Int.toString (!num))
		                      	else
		                          	if (tcheckInt (t2)) then 
		                             	raise Fail "no problem"
		                         	else 
		                             	raise Fail ("Bool type found in Equals operation" ^ " in Statement " ^ Int.toString (!num))

                    else raise  brokenTypes

end





