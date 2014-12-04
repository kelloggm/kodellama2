(* A parser for the Kodellama 2 language *)

(* @ Owner Scelerus *)

(* THIS WOULD ACTUALLY NEED TO EXIST >.<
   OR ELSE I NEED TO MAKE THIS CALL THE COQ
   DIRECTLY
%{

open AbstractSyntax

%}
*)

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
%type <Command.com> com

%left AND
%left OR
%left PLUS MINUS
%left MULT DIV
%left EXP
%left LE LT GE GT EQ
%nonassoc NOT

%%

sexp: STRING				     { StrLit($1) }
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
| NUMBER      				     { (* MAGIC... *) AexpLit($1) }
;

uexp: IDENTIFIER			     { Uexpid($1) }
| IDENTIFIER PLUS uexp			     { Uexpplus($1, $3) }
;

expr : aexp				     { $1 }
| bexp 					     { $1 }
| sexp					     { $1 }
| uexp				     	     { $1 }
;

matchbody : END				     { () }
| WITH expr com matchbody		     { Matchbody($2, $3) }
;

com : SKIP                                   { Skip }
| SET IDENTIFIER TO expr                     { Set($2,$4) } 
| com SEQ com                          	     { Seq($1,$3) }
| IF bexp THEN com ELSE com END              { If($2,$4,$6) }
| IF bexp THEN com END			     { If($2,$4,Skip) }
| WHILE bexp DO com END                      { While($2,$4) }
| LET IDENTIFIER BE expr		     { Let($2,$4) }
| PRINT expr                                 { Print($2) }
| REP aexp TIMES com END		     { Repeat($2, $4) }
| MATCH expr matchbody			     { Match($2, $3) }
| PRINT expr 				     { Print($2) }
| EOF					     { () }
;

