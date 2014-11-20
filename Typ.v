(* Contains the main type system for KodeLlama2 *)

Require Export Aexp.

Module Typ.

Inductive typ :=
  | aexplit : AexpLit -> typ.

End Typ.
