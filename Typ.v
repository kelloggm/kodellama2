(* Contains the main type system for KodeLlama2 *)

Require Export Aexp.
Require Export Bexp.

Module Typ.

(* All typ_s have a bool as their first argument, which represents the
  immutability of the typ; values that start with true can't ever be changed *)

(* We use this inner typ to actually represent typ_s *)
Inductive typ_inner :=
  | aexplit : AexpLit -> typ_inner
  | bexplit : BexpLit -> typ_inner.

(* used by the state to determine if the value is immutable or not *)
Inductive typ :=
  | mk_typ : bool -> typ_inner -> typ.

End Typ.

Export Typ.
