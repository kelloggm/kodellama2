(* Contains the main type system for KodeLlama2 *)

Require Export Exp.

Module Typ.

(* All typ_s have a bool as their first argument, which represents the
  immutability of the typ; values that start with true can't ever be changed *)

(* bool used by the state to determine if the value is immutable or not
   true if immutable, false otherwise *)
Inductive typ :=
  | mk_typ : bool -> ExpLit -> typ.

End Typ.

Export Typ.
