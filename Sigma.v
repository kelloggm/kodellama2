(* This module contains the basic state of a KodeLlama2 program that's running *)

Require Export Typ.
Require Export String.

Module Sigma.

Inductive id :=
  | mk_id : string -> id.

Definition beq_str (s1 s2 : string) : bool :=
  andb (prefix s1 s2) (EqNat.beq_nat (length s1) (length s2)).

Definition beq_id (i1 i2 : id) : bool :=
  match i1, i2 with
    | mk_id s1, mk_id s2 => beq_str s1 s2
  end.

Definition state := id -> typ.

Definition update (sigma : state) (i : id) (t : typ) : state :=
  fun i' => if beq_id i i' then t else sigma i'.

End Sigma.

Export Sigma.
