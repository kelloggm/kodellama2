(* A parser for the Kodellama 2 language *)

(* @ Owner Scelerus *)

%{

open Commands
open Aexp
open Bexp
open BinStringToQ
open BatString

let convert_pair_to_q num denom =
	let sign = if num > 0 then coq_sPos else if num < 0 then coq_sNeg else coq_sZero in
	let posnum = if sign = coq_sNeg then -1*num else num in
	let numStr = String.to_list (Printf.sprintf "X" posnum) in
	let denStr = String.to_list (Printf.sprintf "X" denom) in
	hex_str_to_q numStr denStr sign

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
%type <Commands.coq_Command> com

%left AND
%left OR
%left PLUS MINUS
%left MULT DIV
%left EXP
%left LE LT GE GT EQ
%nonassoc NOT

%%

sexp: STRING				     { StrLit(String.to_list $1) }
| IDENTIFIER				     { StrVar($1) }
| sexp PLUS sexp			     { StrConcat($1, $3) }
;

bexp : TRUE				     { BTrue }
| FALSE					     { BFalse }
| bexp AND bexp				     { BAnd($1, $3) }
| bexp OR bexp				     { BOr($1, $3) }
| NOT bexp				     { BNot($2) }
| expr EQ expr				     { BEq($1, $3) }
| aexp LT aexp				     { BLt($1, $3) }
| aexp LE aexp				     { BLe($1, $3) }
| aexp GT aexp				     { BGt($1, $3) }
| aexp GE aexp				     { BGe($1, $3) }
| IDENTIFIER				     { BVar($1) }
| LPAREN bexp RPAREN			     { $2 }
;

aexp : IDENTIFIER			     { AVar($1) }
| aexp PLUS aexp 			     { APlus($1, $3) }
| aexp MINUS aexp 			     { AMinus($1, $3) }
| aexp MULT aexp 			     { AMult($1, $3) }
| aexp DIV aexp 			     { ADiv($1, $3) }
| aexp EXP aexp 			     { AExp($1, $3) }
| LPAREN aexp RPAREN			     { $2 }
| NUMBER      				     { (* MAGIC... *) ALit($1) }
;

uexp: IDENTIFIER			     { Uexpid($1) }
| IDENTIFIER PLUS uexp			     { Uexpplus($1, $3) }
;

expr : aexp				     { $1 }
| bexp 					     { $1 }
| sexp					     { $1 }
| uexp				     	     { $1 }
;

matchbody : END				     { MBNone }
| WITH expr com matchbody		     { MBSome($2, $3) }
;

com : SKIP                                   { CSkip }
| SET IDENTIFIER TO expr                     { CSet($2,$4) } 
| com SEQ com                          	     { CSeq($1,$3) }
| IF bexp THEN com ELSE com END              { CIf($2,$4,$6) }
| IF bexp THEN com END			     { CIf($2,$4,Skip) }
| WHILE bexp DO com END                      { CWhile($2,$4) }
| LET IDENTIFIER BE expr		     { CLet($2,$4) }
| PRINT expr                                 { CPrint($2) }
| REP aexp TIMES com END		     { CRepeat($2, $4) }
| MATCH expr matchbody			     { CMatch($2, $3) }
| PRINT expr 				     { CPrint($2) }
| EOF					     { () }
;

