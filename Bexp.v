(* This module contains the boolean expression evaluation code
   for KodeLlama2 *)

Module Bexp.

Definition BexpLit := bool.

Inductive Bexp :=
  | Lit: BexpLit -> Bexp
  | Not: Bexp -> Bexp
  | And: Bexp -> Bexp -> Bexp
  | Or: Bexp -> Bexp -> Bexp.

Fixpoint eval_bexp (b: Bexp): BexpLit :=
  match b with
    | Lit b' => b'
    | Not a => match eval_bexp a with
        | true => false
        | false => true
      end
    | And a b => match (eval_bexp a, eval_bexp b) with
        | (true, true) => true
        | _ => false
      end
    | Or a b => match (eval_bexp a, eval_bexp b) with
        | (false, false) => false
        | _ => true
      end
  end.

End Bexp.

Export Bexp.

(** Some tests *)
Example t1_expr := Or (Lit false) (And (Not (Lit false)) (Lit true)).
Example f1_expr := Or (Lit false) (And (Lit false) (Lit true)).
Example t1_eval: eval_bexp t1_expr = true.
Proof. compute; reflexivity. Qed.
Example f1_eval: eval_bexp f1_expr = false.
Proof. compute; reflexivity. Qed.
