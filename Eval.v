(* Contains the code that actually evaluates expressions and commands *)

Require Export Typ.
Require Export Sigma.
Require Export Aexp.
Require Export Bexp.

Module Eval.

Fixpoint eval_aexp (a : Aexp) (sigma : state) :=
  match a with
    | ALit a' => a'
    | APlus b c => plus_aexplit (eval_aexp b sigma) (eval_aexp c sigma)
    | AMinus b c => minus_aexplit (eval_aexp b sigma) (eval_aexp c sigma)
    | AMult b c => mult_aexplit (eval_aexp b sigma) (eval_aexp c sigma)
    | ADiv b c => div_aexplit (eval_aexp b sigma) (eval_aexp c sigma)
    | AExp b c => exp_aexplit (eval_aexp b sigma) (eval_aexp c sigma)
    | ANeg b => neg_aexplit (eval_aexp b sigma)
    | AVar i => match (sigma i) with | mk_typ _ t => match t with | aexplit a' => a' | _ => aexplit_zero (*error!*) end end
  end.

Fixpoint eval_bexp (b: Bexp) (sigma : state) : BexpLit :=
  match b with
    | BLit b' => b'
    | BVar i => match (sigma i) with | mk_typ _ t => match t with | bexplit b' => b' | _ => false (* error! *) end end
    | BNot a => match eval_bexp a sigma with
        | true => false
        | false => true
      end
    | BAnd a b => match (eval_bexp a sigma, eval_bexp b sigma) with
        | (true, true) => true
        | _ => false
      end
    | BOr a b => match (eval_bexp a sigma, eval_bexp b sigma) with
        | (false, false) => false
        | _ => true
      end
  end.

(** Some tests for bexp eval *)
(* I commented them out because who needs tests anyway - Martin *)
(*Example t1_expr := Or (Lit false) (And (Not (Lit false)) (Lit true)).
Example f1_expr := Or (Lit false) (And (Lit false) (Lit true)).
Example t1_eval: eval_bexp t1_expr = true.
Proof. compute; reflexivity. Qed.
Example f1_eval: eval_bexp f1_expr = false.
Proof. compute; reflexivity. Qed.
*)
End Eval.

Export Eval.
