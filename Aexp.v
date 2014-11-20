(* This module contains the arithmetic expression evaluation code
   for KodeLlama2 *)

Module Aexp.

Inductive Aexp :=
  (* Lit has sign, numerator, and denominator *)
  | Lit: bool -> nat -> nat -> Aexp
  | Plus : Aexp -> Aexp -> Aexp
  | Minus : Aexp -> Aexp -> Aexp
  | Mult : Aexp -> Aexp -> Aexp
  | Div : Aexp -> Aexp -> Aexp
  | Exp : Aexp -> Aexp -> Aexp
  | Neg : Aexp -> Aexp.


End Aexp.
