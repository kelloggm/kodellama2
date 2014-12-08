(* This module contains the basic state of a KodeLlama2 program that's running *)

Require Export Typ.
Require Export Ident.

Module Sigma.

Definition state := ident -> typ.

Definition update (sigma : state) (i : ident) (t : typ) : state :=
  fun i' => if beq_ident i i' 
    then match (sigma i) with
      | mk_typ b t' => 
        match t' with
          | exp_error => t
          | _ => if b then (sigma i) else t
        end
      end
    else sigma i'.

Definition initial_state (i: ident) := 
  mk_typ true exp_error.

End Sigma.

Export Sigma.
