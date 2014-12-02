(* This module contains the arithmetic expression evaluation code
   for KodeLlama2 *)

Module Aexp.

Require Export QArith.
Require Export Ident.

Inductive AexpLit :=
  (* Lit has sign, numerator, and denominator *)
  | mk_aexp_lit : Q -> AexpLit
  | aexp_error : AexpLit.

Definition plus_aexplit (a b : AexpLit) := 
  match a, b with
    | mk_aexp_lit q, mk_aexp_lit p => mk_aexp_lit (Qplus q p)
    | _, _ => aexp_error
  end.

Definition minus_aexplit (a b : AexpLit) :=  
  match a, b with
    | mk_aexp_lit q, mk_aexp_lit p => mk_aexp_lit (Qminus q p)
    | _, _ => aexp_error
   end.

Definition mult_aexplit (a b : AexpLit) := 
 match a, b with
    | mk_aexp_lit q, mk_aexp_lit p => mk_aexp_lit (Qmult q p)
    | _, _ => aexp_error
  end.

Definition div_aexplit (a b : AexpLit) := 
 match a, b with
    | mk_aexp_lit q, mk_aexp_lit p => 
       if Qeq_bool p (Qmake 0 1) then aexp_error else mk_aexp_lit (Qdiv q p)
    | _, _ => aexp_error
  end.

Definition exp_aexplit (a b : AexpLit) := mk_aexp_lit (Qmake 1 1). (* exponentiation...seems hard. We'll get to it later *)

Definition neg_aexplit (a : AexpLit) := 
  match a with
    | mk_aexp_lit q => mk_aexp_lit (Qinv q)
    | aexp_error => aexp_error
  end.

Definition aexplit_zero := mk_aexp_lit (Qmake 0 1).

Inductive Aexp :=
  | ALit: AexpLit -> Aexp
  | AVar: ident -> Aexp
  | APlus : Aexp -> Aexp -> Aexp
  | AMinus : Aexp -> Aexp -> Aexp
  | AMult : Aexp -> Aexp -> Aexp
  | ADiv : Aexp -> Aexp -> Aexp
  | AExp : Aexp -> Aexp -> Aexp
  | ANeg : Aexp -> Aexp.

End Aexp.

Export Aexp.
