%{

(* A parser for the Kodellama 2 language *)

(* @ Owner Scelerus *)

open Aexp
open Exp
open BinStringToQ

let debug = ref false

let print_string_debug s = if !debug then print_string s else ()

(* Adapted from ocaml FAQ *)
let string_to_list str =
	let rec exp i l =
		if i < 0 then l else exp (i-1) ((String.get str i) :: l) in
	exp ((String.length str) - 1) []

let convert_pair_to_q num denom =
	let sign = if num > 0 then BinStringToQ.Coq_sPos else if num < 0 then BinStringToQ.Coq_sNeg else BinStringToQ.Coq_sZero in
	let posnum = if sign = BinStringToQ.Coq_sNeg then -1*num else num in
	let numStr = string_to_list (Printf.sprintf "%X" posnum) in
	let denStr = string_to_list (Printf.sprintf "%X" denom) in
	BinStringToQ.hex_str_to_q numStr denStr sign

let string_to_q str =
	try
		let ind = String.index str '.' in
		let denompow = (String.length str) - ind - 1 in
		let numerator = String.concat "" [(String.sub str 0 ind) ; (String.sub str (ind + 1) ((String.length str) - ind -1))] in
		convert_pair_to_q (int_of_string numerator) (int_of_float ((float_of_int 10) ** (float_of_int denompow)))
	with Not_found ->
		convert_pair_to_q (int_of_string str) 1

let linenoError = ref 0

let parse_error s = print_string ("There was a syntax error after line " ^ (string_of_int !linenoError) ^ "\n")

%}


%token <string>         IDENTIFIER
%token <string>         NUMBER
%token <string> 	     STRING

%token <int> PLUS 
%token <int> MINUS
%token <int> MULT
%token <int> DIV 
%token <int> SEQ
%token <int> EXP
%token <int> MOD
%token <int> TRUE
%token <int> FALSE
%token <int> COLON
%token <int> EQ
%token <int> LT
%token <int> LE
%token <int> GT
%token <int> GE
%token <int> SKIP
%token <int> SET
%token <int> TO
%token <int> LET
%token <int> BE
%token <int> IF
%token <int> THEN
%token <int> ELSE
%token <int> MATCH
%token <int> WITH
%token <int> END
%token <int> PRINT
%token <int> WHILE
%token <int> DO
%token <int> REP
%token <int> TIMES
%token <int> RPAREN
%token <int> LPAREN
%token <int> AND
%token <int> OR
%token <int> NOT
%token EOF

%start com
%type < Commands.Commands.coq_Command > com

%left AND
%left OR
%left PLUS MINUS
%left MULT DIV
%left EXP
%left LE LT GE GT EQ
%nonassoc NOT

%%

uexp: IDENTIFIER			     { Exp.Uexpid(string_to_list $1) }
| IDENTIFIER PLUS expr			     { linenoError := $2; Exp.Uexpplus(Exp.Uexpid (string_to_list $1), $3) }
;

sexp: STRING				     { Exp.SLit(Exp.Coq_mk_sexp_lit (string_to_list $1)) }
| IDENTIFIER				     { Exp.SVar(string_to_list $1) }
| sexp PLUS sexp			     { linenoError := $2; Exp.SConcat($1, $3) }
;

bexp : TRUE				     { linenoError := $1; Exp.BLit (Exp.Coq_mk_bexp_lit true) }
| FALSE					     { linenoError := $1; Exp.BLit (Exp.Coq_mk_bexp_lit false) }
| bexp AND bexp				     { linenoError := $2; Exp.BAnd($1, $3) }
| bexp OR bexp				     { linenoError := $2; Exp.BOr($1, $3) }
| NOT bexp				     { linenoError := $1; Exp.BNot($2) }
| expr EQ expr				     { linenoError := $2; Exp.BEq($1, $3) }
| aexp LT aexp				     { linenoError := $2; Exp.BLt($1, $3) }
| aexp LE aexp				     { linenoError := $2; Exp.BLe($1, $3) }
| aexp GT aexp				     { linenoError := $2; Exp.BGt($1, $3) }
| aexp GE aexp				     { linenoError := $2; Exp.BGe($1, $3) }
| IDENTIFIER				     { Exp.BVar(string_to_list $1) }
| LPAREN bexp RPAREN			     { linenoError := $3; $2 }
;

aexp : IDENTIFIER			     { Exp.AVar(string_to_list $1) }
| aexp PLUS aexp 			     { linenoError := $2; Exp.APlus($1, $3) }
| aexp MINUS aexp 			     { linenoError := $2; Exp.AMinus($1, $3) }
| aexp MULT aexp 			     { linenoError := $2; Exp.AMult($1, $3) }
| aexp DIV aexp 			     { linenoError := $2; Exp.ADiv($1, $3) }
| aexp EXP aexp 			     { linenoError := $2; Exp.AExp($1, $3) }
| aexp MOD aexp 			     { linenoError := $2; Exp.AMod($1, $3) }
| LPAREN aexp RPAREN			     { linenoError := $3; $2 }
| NUMBER      				     { let myq = string_to_q $1 in
									match myq with
										| Some q -> Exp.ALit(Exp.Coq_mk_aexp_lit q)
										| None -> Exp.ALit(Exp.Coq_aexp_error) }
;

expr : aexp				     { Exp.EAexp $1 }
| bexp 					     { Exp.EBexp $1 }
| sexp					     { Exp.ESexp $1 }
| uexp				     	 { Exp.EUexp $1 }
;

matchbody : END				     { linenoError := $1; Commands.Commands.MBNone }
| SEQ END				     { linenoError := $2; Commands.Commands.MBNone }
| WITH expr COLON com matchbody		     { linenoError := $3; print_string_debug "first with " ; Commands.Commands.MBSome($2, $4, $5) }
| WITH expr COLON com SEQ matchbody		     { linenoError := $3; print_string_debug "second with " ; Commands.Commands.MBSome($2, $4, $6) }

;

com : SKIP                                   { linenoError := $1; print_string_debug "skip " ; Commands.Commands.CSkip }
| SET IDENTIFIER TO expr                     { linenoError := $3; print_string_debug "set " ; Commands.Commands.CSet(string_to_list $2,$4) } 
| IF bexp THEN com ELSE com END              { linenoError := $7; print_string_debug "if 1 " ; Commands.Commands.CIf($2,$4,$6) }
| IF bexp THEN com END			     { linenoError := $5; print_string_debug "if 2 " ; Commands.Commands.CIf($2,$4,Commands.Commands.CSkip) }
| WHILE bexp DO com END                      { linenoError := $5; print_string_debug "while 1 " ; Commands.Commands.CWhile($2,$4) }
| WHILE bexp DO SEQ com END                  { linenoError := $6; print_string_debug "while 2 " ;Commands.Commands.CWhile($2,$5) }
| WHILE bexp DO SEQ com SEQ END              { linenoError := $7; print_string_debug "while 3 " ;Commands.Commands.CWhile($2,$5) }
| WHILE bexp DO com SEQ END                  { linenoError := $6; print_string_debug "while 4 " ;Commands.Commands.CWhile($2,$4) }
| LET IDENTIFIER BE expr		     { linenoError := $3; print_string_debug "let " ; Commands.Commands.CLet(string_to_list $2,$4) }
| PRINT expr                                 { linenoError := $1; print_string_debug "print " ; Commands.Commands.CPrint($2) }
| REP aexp TIMES COLON com END		     { linenoError := $6; print_string_debug "rep 1 " ; Commands.Commands.CRepeat($2, $5) }
| REP aexp TIMES COLON SEQ com END		     { linenoError := $7; print_string_debug "rep 2 " ; Commands.Commands.CRepeat($2, $6) }
| REP aexp TIMES COLON SEQ com SEQ END	     { linenoError := $8; print_string_debug "rep 3 " ; Commands.Commands.CRepeat($2, $6) }
| REP aexp TIMES COLON com SEQ END		     { linenoError := $7; print_string_debug "rep 4 " ; Commands.Commands.CRepeat($2, $5) }
| MATCH expr matchbody			     { linenoError := $1; print_string_debug "match 1 " ; Commands.Commands.CMatch($2, $3) }
| MATCH expr SEQ matchbody		     { linenoError := $1; print_string_debug "match 2 " ; Commands.Commands.CMatch($2, $4) }
| com SEQ com                          	     { linenoError := $2; print_string_debug "seq " ; Commands.Commands.CSeq($1,$3) }
| EOF				     { print_string_debug "eof-skip " ; Commands.Commands.CSkip }
;

