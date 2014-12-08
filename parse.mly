%{

(* A parser for the Kodellama 2 language *)

(* @ Owner Scelerus *)

open Aexp
open Exp
open BinStringToQ

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

%}


%token <string>         IDENTIFIER
%token <string>         NUMBER
%token <string> 	STRING

%token PLUS
%token MINUS
%token MULT
%token DIV
%token SEQ
%token EXP
%token TRUE
%token FALSE
%token EQ
%token LT
%token LE
%token GT
%token GE
%token SKIP
%token SET
%token TO
%token LET
%token BE
%token IF
%token THEN
%token ELSE
%token MATCH
%token WITH
%token END
%token PRINT
%token WHILE
%token DO
%token REP
%token TIMES
%token RPAREN
%token LPAREN
%token AND
%token OR
%token NOT
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
| IDENTIFIER PLUS uexp			     { Exp.Uexpplus(Exp.Uexpid (string_to_list $1), $3) }
;

sexp: STRING				     { Exp.SLit(Exp.Coq_mk_sexp_lit (string_to_list $1)) }
| IDENTIFIER				     { Exp.SVar(string_to_list $1) }
| sexp PLUS sexp			     { Exp.SConcat($1, $3) }
;

bexp : TRUE				     { Exp.BLit (Exp.Coq_mk_bexp_lit true) }
| FALSE					     { Exp.BLit (Exp.Coq_mk_bexp_lit false) }
| bexp AND bexp				     { Exp.BAnd($1, $3) }
| bexp OR bexp				     { Exp.BOr($1, $3) }
| NOT bexp				     { Exp.BNot($2) }
| expr EQ expr				     { Exp.BEq($1, $3) }
| aexp LT aexp				     { Exp.BLt($1, $3) }
| aexp LE aexp				     { Exp.BLe($1, $3) }
| aexp GT aexp				     { Exp.BGt($1, $3) }
| aexp GE aexp				     { Exp.BGe($1, $3) }
| IDENTIFIER				     { Exp.BVar(string_to_list $1) }
| LPAREN bexp RPAREN			     { $2 }
;

aexp : IDENTIFIER			     { Exp.AVar(string_to_list $1) }
| aexp PLUS aexp 			     { Exp.APlus($1, $3) }
| aexp MINUS aexp 			     { Exp.AMinus($1, $3) }
| aexp MULT aexp 			     { Exp.AMult($1, $3) }
| aexp DIV aexp 			     { Exp.ADiv($1, $3) }
| aexp EXP aexp 			     { Exp.AExp($1, $3) }
| LPAREN aexp RPAREN			     { $2 }
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

matchbody : END				     { Commands.Commands.MBNone }
| WITH expr com matchbody		     { Commands.Commands.MBSome($2, $3, $4) }
;

com : SKIP                                   { Commands.Commands.CSkip }
| SET IDENTIFIER TO expr                     { Commands.Commands.CSet(string_to_list $2,$4) } 
| com SEQ com                          	     { Commands.Commands.CSeq($1,$3) }
| IF bexp THEN com ELSE com END              { Commands.Commands.CIf($2,$4,$6) }
| IF bexp THEN com END			     { Commands.Commands.CIf($2,$4,Commands.Commands.CSkip) }
| WHILE bexp DO com END                      { Commands.Commands.CWhile($2,$4) }
| LET IDENTIFIER BE expr		     { Commands.Commands.CLet(string_to_list $2,$4) }
| PRINT expr                                 { Commands.Commands.CPrint($2) }
| REP aexp TIMES com END		     { Commands.Commands.CRepeat($2, $4) }
| MATCH expr matchbody			     { Commands.Commands.CMatch($2, $3) }
| EOF					     { Commands.Commands.CSkip }
;

