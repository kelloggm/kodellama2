(* Kodellama 2 lexer *)

(* @ Owner Scelerus *)


{
open Parse
open Printf

let lineno = ref 1

}



let blank = [' ' '\012' '\t']

(* this is "program" in the grammar on the whiteboard *)
rule initial = parse
    "//"     { endline lexbuf }
    | blank  { initial lexbuf }
    | '\n'     	       { lineno := !lineno + 1 ; initial lexbuf }
    | ';'     	       { print_string "SEQ " ; SEQ(!lineno) }
    | '+'      	       { print_string "PLUS " ; PLUS(!lineno) }
    | '-'     	       { print_string "MINUS " ; MINUS(!lineno) }
    | '*'    	       { print_string "MULT " ; MULT(!lineno) }
    | '/'      	       { print_string "DIV " ; DIV(!lineno) }
    | '^'      	       { print_string "EXP " ; EXP(!lineno) }
    | ':'      	       { print_string "COLON " ; COLON(!lineno) }
    | "true"   	       { print_string "TRUE " ; TRUE(!lineno) }
    | "false"  	       { print_string "FALSE " ; FALSE(!lineno) }
    | "is"
    | '='      	       { print_string "EQ " ; EQ(!lineno) }
    | '<'	       { print_string "LT " ; LT(!lineno) }
    | '>'	       { print_string "GT " ; GT(!lineno) }
    | ">="	       { print_string "GE " ; GE(!lineno) }
    | "<="	       { print_string "LE " ; LE(!lineno) }
    | "and"	       { print_string "AND " ; AND(!lineno) }
    | "or"	       { print_string "OR " ; OR(!lineno) }
    | "not"	       { print_string "NOT " ; NOT(!lineno) }
    | "skip"	       { print_string "SKIP " ; SKIP(!lineno) }
    | "set"	       { print_string "SET " ; SET(!lineno) }
    | "to"	       { print_string "TO " ; TO(!lineno) }
    | "let"	       { print_string "LET " ; LET(!lineno) }
    | "be"	       { print_string "BE " ; BE(!lineno) }
    | "if"	       { print_string "IF " ; IF(!lineno) }
    | "then"	       { print_string "THEN " ; THEN(!lineno) }
    | "else"	       { print_string "ELSE " ; ELSE(!lineno) }
    | "print"	       { print_string "PRINT " ; PRINT(!lineno) }
    | "match"	       { print_string "MATCH " ; MATCH(!lineno) }
    | "with"	       { print_string "WITH " ; WITH(!lineno) }
    | "end"	       { print_string "END " ; END(!lineno) }
    | "while"	       { print_string "WHILE " ; WHILE(!lineno) }
    | "do"	       { print_string "DO " ; DO(!lineno) }
    | "repeat"	       { print_string "REP " ; REP(!lineno) }
    | "times"	       { print_string "TIMES " ; TIMES(!lineno) }
    | '('	       { print_string "LPAREN " ; LPAREN(!lineno) }
    | ')'	       { print_string "RPAREN " ; RPAREN(!lineno) }
    | ['A'-'Z''a'-'z''_']['0'-'9''A'-'Z''a'-'z''_']*{
			let str = Lexing.lexeme lexbuf in 
  			print_string "IDENTIFIER " ; IDENTIFIER(str)
  			}
    | ['\"']([^'\"''\n'])*['\"'] {
      		     let str = Lexing.lexeme lexbuf in
		     print_string "STRING " ; STRING(String.sub str 1 ((String.length str) - 2))
		     }
		     
    | ['-']?['0'-'9']+(['.']['0'-'9']+)? {
      					     let str = Lexing.lexeme lexbuf in
					     print_string "NUMBER " ; NUMBER(str)
					     }

    | _       {
      	      Printf.printf ("You have a character Kodellama does not know on line %d : '%s'\n") (!lineno) (Lexing.lexeme lexbuf) ;
  	      exit 1 
	      }					     

    | eof		{ print_string "EOF " ; EOF }


and endline = parse
    '\n'    { lineno := !lineno + 1 ; initial lexbuf}
    | _     { endline lexbuf}
    | eof   { EOF }
