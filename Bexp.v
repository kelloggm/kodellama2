(* This module contains the boolean expression evaluation code
   for KodeLlama2 *)

Require Export Ident.

Module Bexp.

Inductive BexpLit :=
  | mk_bexp_lit : bool -> BexpLit
  | bexp_error : BexpLit.

Inductive Bexp :=
  | BLit: BexpLit -> Bexp
  | BNot: Bexp -> Bexp
  | BAnd: Bexp -> Bexp -> Bexp
  | BOr: Bexp -> Bexp -> Bexp
  | BVar : ident -> Bexp.
  
End Bexp.

Export Bexp.

