(* Kodellama identifiers are strings! *)

Require Export String.

Module Ident.

Inductive ident :=
  | mk_id : string -> ident.

Definition beq_str (s1 s2 : string) : bool :=
  andb (prefix s1 s2) (EqNat.beq_nat (length s1) (length s2)).

Definition beq_ident (i1 i2 : ident) : bool :=
  match i1, i2 with
    | mk_id s1, mk_id s2 => beq_str s1 s2
  end.

End Ident.

Export Ident.
