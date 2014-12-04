(* This module contains code for arbitrary expressions in Kodellama *)

Require Export QArith.
Require Export Ident.

Module Exp.

Inductive ExpLit :=
  | mk_explit_from_aexp: AexpLit -> ExpLit
  | mk_explit_from_bexp: BexpLit -> ExpLit
with AexpLit :=
  (* Lit has sign, numerator, and denominator *)
  | mk_aexp_lit : Q -> AexpLit
  | aexp_error : AexpLit
with BexpLit :=
  | mk_bexp_lit : bool -> BexpLit
  | bexp_error : BexpLit.

Inductive Exp :=
  | EAexp: Aexp -> Exp
  | EBexp: Bexp -> Exp
with Aexp :=
  | ALit: AexpLit -> Aexp
  | AVar: ident -> Aexp
  | APlus : Aexp -> Aexp -> Aexp
  | AMinus : Aexp -> Aexp -> Aexp
  | AMult : Aexp -> Aexp -> Aexp
  | ADiv : Aexp -> Aexp -> Aexp
  | AExp : Aexp -> Aexp -> Aexp
  | ANeg : Aexp -> Aexp
with Bexp :=
  | BLit: BexpLit -> Bexp
  | BNot: Bexp -> Bexp
  | BAnd: Bexp -> Bexp -> Bexp
  | BOr: Bexp -> Bexp -> Bexp
  | BVar : ident -> Bexp
  | BEq: Exp -> Exp -> Bexp
  | BLt: Aexp -> Aexp -> Bexp
  | BGt: Aexp -> Aexp -> Bexp.

End Exp.

Export Exp.