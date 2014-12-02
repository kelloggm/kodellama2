(* This module contains code for arbitrary expressions in Kodellama *)

Require Export Aexp.
Require Export Bexp.

Module Exp.

Inductive ExpLit :=
  | mk_explit_from_aexp: AexpLit -> ExpLit
  | mk_explit_from_bexp: BexpLit -> ExpLit.

Inductive Exp :=
  | EAexp: Aexp -> Exp
  | EBexp: Bexp -> Exp.

End Exp.

Export Exp.