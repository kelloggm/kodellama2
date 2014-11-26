(* This module contains the boolean expression evaluation code
   for KodeLlama2 *)

Module Bexp.

Definition BexpLit := bool.

Inductive Bexp :=
  | Lit: BexpLit -> Bexp
  | Not: Bexp -> Bexp
  | And: Bexp -> Bexp -> Bexp
  | Or: Bexp -> Bexp -> Bexp.

End Bexp.

Export Bexp.

