(* Kodellama 2 lexer *)

(* @ Owner Scelerus *)

(*
{
open Parse
}
*)


let blank = [' ' '\012' '\t']

(* this is "program" in the grammar on the whiteboard *)
rule initial = parse
    "//"     { endline lexbuf }
    | blank  { initial lexbuf }
    | '\n'     	       { SEQ }
    | '+'      	       { PLUS }
    | '-'     	       { MINUS }
    | '*'    	       { MULT }
    | '/'      	       { DIV }
    | '^'      	       { EXP }
    | "true"   	       { TRUE }
    | "false"  	       { FALSE }
    | "is"
    | '='      	       { EQ }
    | '<'	       { LT }
    | '>'	       { GT }
    | ">="	       { GE }
    | "<="	       { LE }
    | "and"	       { AND }
    | "or"	       { OR }
    | "not"	       { NOT }
    | "skip"	       { SKIP }
    | "set"	       { SET }
    | "to"	       { TO }
    | "let"	       { LET }
    | "be"	       { BE }
    | "if"	       { IF }
    | "then"	       { THEN }
    | "else"	       { ELSE }
    | "print"	       { PRINT }
    | "match"	       { MATCH }
    | "with"	       { WITH }
    | "end"	       { END }
    | "while"	       { WHILE }
    | "do"	       { DO }
    | "repeat"	       { REP }
    | "times:"	       { TIMES }
    | '('	       { LPAREN }
    | ')'	       { RPAREN }
    | ['A'-'Z''a'-'z''_']['0'-'9''A'-'Z''a'-'z''_']*{
			let str = Lexing.lexeme lexbuf in 
  			IDENTIFIER(str)
  			}
    | \"([^"^\n])*\" {
      		     let str = Lexing.lexeme lexbuf in
		     STRING(str)
		     }
    | ((([0-9])+(\.?)([0-9])*)|(\.)([0-9]+)) {
      					     let str = Lexing.lexeme lexbuf in
					     NUMBER(str)
					     }
    | eof		{ EOF }
    | _       { 
      	      Printf.printf "You've got a character Kodellama doesn't know : '%s'\n" (Lexing.lexeme lexbuf) ;
  	      exit 1 }
 

and endline = parse
    '\n'    { initial lexbuf}
    | _     { endline lexbuf}
    | eof   { EOF }
