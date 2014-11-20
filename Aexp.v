(* This module contains the arithmetic expression evaluation code
   for KodeLlama2 *)

Module Aexp.

Inductive AexpLit :=
  (* Lit has sign, numerator, and denominator *)
  | mk_aexp_lit : bool -> nat -> nat -> AexpLit.

Inductive Aexp :=
  | Lit: AexpLit -> Aexp
  | Plus : Aexp -> Aexp -> Aexp
  | Minus : Aexp -> Aexp -> Aexp
  | Mult : Aexp -> Aexp -> Aexp
  | Div : Aexp -> Aexp -> Aexp
  | Exp : Aexp -> Aexp -> Aexp
  | Neg : Aexp -> Aexp.


End Aexp.

Export Aexp.
