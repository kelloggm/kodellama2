(* Kodellama 2 lexer *)

(* @ Owner Scelerus *)


{
open Parse
open Printf
}



let blank = [' ' '\012' '\t' '\n']

(* this is "program" in the grammar on the whiteboard *)
rule initial = parse
    "//"     { endline lexbuf }
    | blank  { initial lexbuf }
    | ';'     	       { print_string "SEQ " ; SEQ }
    | '+'      	       { print_string "PLUS " ; PLUS }
    | '-'     	       { print_string "MINUS " ; MINUS }
    | '*'    	       { print_string "MULT " ; MULT }
    | '/'      	       { print_string "DIV " ; DIV }
    | '^'      	       { print_string "EXP " ; EXP }
    | "true"   	       { print_string "TRUE " ; TRUE }
    | "false"  	       { print_string "FALSE " ; FALSE }
    | "is"
    | '='      	       { print_string "EQ " ; EQ }
    | '<'	       { print_string "LT " ; LT }
    | '>'	       { print_string "GT " ; GT }
    | ">="	       { print_string "GE " ; GE }
    | "<="	       { print_string "LE " ; LE }
    | "and"	       { print_string "AND " ; AND }
    | "or"	       { print_string "OR " ; OR }
    | "not"	       { print_string "NOT " ; NOT }
    | "skip"	       { print_string "SKIP " ; SKIP }
    | "set"	       { print_string "SET " ; SET }
    | "to"	       { print_string "TO " ; TO }
    | "let"	       { print_string "LET " ; LET }
    | "be"	       { print_string "BE " ; BE }
    | "if"	       { print_string "IF " ; IF }
    | "then"	       { print_string "THEN " ; THEN }
    | "else"	       { print_string "ELSE " ; ELSE }
    | "print"	       { print_string "PRINT " ; PRINT }
    | "match"	       { print_string "MATCH " ; MATCH }
    | "with"	       { print_string "WITH " ; WITH }
    | "end"	       { print_string "END " ; END }
    | "while"	       { print_string "WHILE " ; WHILE }
    | "do"	       { print_string "DO " ; DO }
    | "repeat"	       { print_string "REP " ; REP }
    | "times:"	       { print_string "TIMES " ; TIMES }
    | '('	       { print_string "LPAREN " ; LPAREN }
    | ')'	       { print_string "RPAREN " ; RPAREN }
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
      	      Printf.printf ("You have a character Kodellama does not know : '%s'\n") (Lexing.lexeme lexbuf) ;
  	      exit 1 
	      }					     

    | eof		{ print_string "EOF " ; EOF }


and endline = parse
    '\n'    { initial lexbuf}
    | _     { endline lexbuf}
    | eof   { EOF }
