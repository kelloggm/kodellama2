(* Kodellama 2 lexer *)

(* @ Owner Scelerus *)


{
open Parse
open Printf

let lineno = ref 1

let debug = ref false

let print_string_debug s =
    if !debug then print_string s else ()

}



let blank = [' ' '\012' '\t']

(* this is "program" in the grammar on the whiteboard *)
rule initial = parse
    "//"     { endline lexbuf }
    | blank  { initial lexbuf }
    | '\n'     	       { lineno := !lineno + 1 ; initial lexbuf }
    | ';'     	       { print_string_debug "SEQ " ; SEQ(!lineno) }
    | '+'      	       { print_string_debug "PLUS " ; PLUS(!lineno) }
    | '-'     	       { print_string_debug "MINUS " ; MINUS(!lineno) }
    | '*'    	       { print_string_debug "MULT " ; MULT(!lineno) }
    | '/'      	       { print_string_debug "DIV " ; DIV(!lineno) }
    | '^'      	       { print_string_debug "EXP " ; EXP(!lineno) }
    | '%'	       { print_string_debug "MOD" ; MOD(!lineno) }
    | ':'      	       { print_string_debug "COLON " ; COLON(!lineno) }
    | "true"   	       { print_string_debug "TRUE " ; TRUE(!lineno) }
    | "false"  	       { print_string_debug "FALSE " ; FALSE(!lineno) }
    | "is"
    | '='      	       { print_string_debug "EQ " ; EQ(!lineno) }
    | '<'	       { print_string_debug "LT " ; LT(!lineno) }
    | '>'	       { print_string_debug "GT " ; GT(!lineno) }
    | ">="	       { print_string_debug "GE " ; GE(!lineno) }
    | "<="	       { print_string_debug "LE " ; LE(!lineno) }
    | "and"	       { print_string_debug "AND " ; AND(!lineno) }
    | "or"	       { print_string_debug "OR " ; OR(!lineno) }
    | "not"	       { print_string_debug "NOT " ; NOT(!lineno) }
    | "skip"	       { print_string_debug "SKIP " ; SKIP(!lineno) }
    | "set"	       { print_string_debug "SET " ; SET(!lineno) }
    | "to"	       { print_string_debug "TO " ; TO(!lineno) }
    | "let"	       { print_string_debug "LET " ; LET(!lineno) }
    | "be"	       { print_string_debug "BE " ; BE(!lineno) }
    | "if"	       { print_string_debug "IF " ; IF(!lineno) }
    | "then"	       { print_string_debug "THEN " ; THEN(!lineno) }
    | "else"	       { print_string_debug "ELSE " ; ELSE(!lineno) }
    | "print"	       { print_string_debug "PRINT " ; PRINT(!lineno) }
    | "match"	       { print_string_debug "MATCH " ; MATCH(!lineno) }
    | "with"	       { print_string_debug "WITH " ; WITH(!lineno) }
    | "end"	       { print_string_debug "END " ; END(!lineno) }
    | "while"	       { print_string_debug "WHILE " ; WHILE(!lineno) }
    | "do"	       { print_string_debug "DO " ; DO(!lineno) }
    | "repeat"	       { print_string_debug "REP " ; REP(!lineno) }
    | "times"	       { print_string_debug "TIMES " ; TIMES(!lineno) }
    | '('	       { print_string_debug "LPAREN " ; LPAREN(!lineno) }
    | ')'	       { print_string_debug "RPAREN " ; RPAREN(!lineno) }
    | ['A'-'Z''a'-'z''_']['0'-'9''A'-'Z''a'-'z''_']*{
			let str = Lexing.lexeme lexbuf in 
  			print_string_debug "IDENTIFIER " ; IDENTIFIER(str)
  			}
    | ['\"']([^'\"''\n'])*['\"'] {
      		     let str = Lexing.lexeme lexbuf in
		     print_string_debug "STRING " ; STRING(String.sub str 1 ((String.length str) - 2))
		     }
		     
    | ['-']?['0'-'9']+(['.']['0'-'9']+)? {
      					     let str = Lexing.lexeme lexbuf in
					     print_string_debug "NUMBER " ; NUMBER(str)
					     }

    | _       {
      	      Printf.printf ("You have a character Kodellama does not know on line %d : '%s'\n") (!lineno) (Lexing.lexeme lexbuf) ;
  	      exit 1 
	      }					     

    | eof		{ print_string_debug "EOF " ; EOF }


and endline = parse
    '\n'    { lineno := !lineno + 1 ; initial lexbuf}
    | _     { endline lexbuf}
    | eof   { EOF }
