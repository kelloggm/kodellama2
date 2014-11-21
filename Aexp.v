(* This module contains the arithmetic expression evaluation code
   for KodeLlama2 *)

Module Aexp.

Require Export QArith.

Inductive AexpLit :=
  (* Lit has sign, numerator, and denominator *)
  | mk_aexp_lit : Q -> AexpLit.

SearchAbout Q.

Definition plus_aexplit (a b : AexpLit) := 
  match a, b with
    | mk_aexp_lit q, mk_aexp_lit p => mk_aexp_lit (Qplus q p)
  end.

Definition minus_aexplit (a b : AexpLit) :=  
  match a, b with
    | mk_aexp_lit q, mk_aexp_lit p => mk_aexp_lit (Qminus q p)
  end.

Definition mult_aexplit (a b : AexpLit) := 
 match a, b with
    | mk_aexp_lit q, mk_aexp_lit p => mk_aexp_lit (Qmult q p)
  end.

Definition div_aexplit (a b : AexpLit) := 
 match a, b with
    | mk_aexp_lit q, mk_aexp_lit p => 
       if Qeq_bool p (Qmake 0 1) then mk_aexp_lit (Qmake 0 1) else mk_aexp_lit (Qdiv q p)
  end.

Definition exp_aexplit (a b : AexpLit) := mk_aexp_lit (Qmake 1 1). (* exponentiation...seems hard. We'll get to it later *)

Definition neg_aexplit (a : AexpLit) := 
  match a with
    | mk_aexp_lit q => mk_aexp_lit (Qinv q)
  end.

Inductive Aexp :=
  | Lit: AexpLit -> Aexp
  | Plus : Aexp -> Aexp -> Aexp
  | Minus : Aexp -> Aexp -> Aexp
  | Mult : Aexp -> Aexp -> Aexp
  | Div : Aexp -> Aexp -> Aexp
  | Exp : Aexp -> Aexp -> Aexp
  | Neg : Aexp -> Aexp.

Fixpoint eval_aexp (a : Aexp) :=
  match a with
    | Lit a' => a'
    | Plus b c => plus_aexplit (eval_aexp b) (eval_aexp c)
    | Minus b c => minus_aexplit (eval_aexp b) (eval_aexp c)
    | Mult b c => mult_aexplit (eval_aexp b) (eval_aexp c)
    | Div b c => div_aexplit (eval_aexp b) (eval_aexp c)
    | Exp b c => exp_aexplit (eval_aexp b) (eval_aexp c)
    | Neg b => neg_aexplit (eval_aexp b)
  end.

End Aexp.

Export Aexp.
