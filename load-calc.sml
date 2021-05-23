structure CalcLrVals = CalcLrValsFun(structure Token = LrParser.Token)
structure CalcLex = CalcLexFun(structure Tokens = CalcLrVals.Tokens);
structure CalcParser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = CalcLrVals.ParserData
     	       structure Lex = CalcLex)
     
fun invoke lexstream =
    	     	let fun print_error (s,pos:int,_) =
		    	TextIO.output(TextIO.stdOut, "Error, line " ^ (Int.toString pos) ^ "," ^ s ^ "\n")
		in
		    CalcParser.parse(0,lexstream,print_error,())
		end

fun stringToLexer str =
    let val done = ref false
    	val lexer=  CalcParser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
	lexer
    end	
		
fun parse (lexer) =
    let val dummyEOF = CalcLrVals.Tokens.EOF(0,0)
    	val (result, lexer) = invoke lexer
	val (nextToken, lexer) = CalcParser.Stream.get lexer
    in
        if CalcParser.sameToken(nextToken, dummyEOF) then result
 	else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); result)
    end

fun read_file (infile) = 
    
    let 
        val instream = TextIO.openIn infile
        fun loop instream = 
            case TextIO.inputLine instream of 
                SOME line   => line ^ loop instream 
            |   NONE        => ""
    in 
        loop instream before TextIO.closeIn instream
    end;




val parseString = parse o stringToLexer o read_file 

val infile = valOf (TextIO.inputLine TextIO.stdIn);
val parse_output = parseString (String.substring (infile, 0, String.size (infile) - 1));
val output = evalPrgm (parse_output);