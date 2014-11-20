(* This module contains the arithmetic expression evaluation code
   for KodeLlama2 *)

Module Aexp.

Inductive AexpLit :=
  (* Lit has sign, numerator, and denominator *)
  | mk_aexp_lit : bool -> nat -> nat -> AexpLit.

Definition plus_aexplit (a b : AexpLit) := mk_aexp_lit false 0 1.

Definition minus_aexplit (a b : AexpLit) := mk_aexp_lit false 0 1.

Definition mult_aexplit (a b : AexpLit) := mk_aexp_lit false 0 1.

Definition div_aexplit (a b : AexpLit) := mk_aexp_lit false 0 1.

Definition exp_aexplit (a b : AexpLit) := mk_aexp_lit false 0 1.

Definition neg_aexplit (a : AexpLit) := mk_aexp_lit false 0 1.

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
