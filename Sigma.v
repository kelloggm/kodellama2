(* This module contains the basic state of a KodeLlama2 program that's running *)

Require Export Typ.

Module Sigma.

Inductive id :=
  | mk_id : string -> id.

Definition state := id -> typ.

Definition update (sigma : state) (i : id) (t : typ) : state :=
  fun i' => if i = i' then t else sigma i'..

End Sigma.
