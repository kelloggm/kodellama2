(* Contains the code that actually evaluates expressions and commands *)

Require Export Typ.
Require Export Sigma.
Require Export Aexp.
Require Export Bexp.
Require Export Commands.

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
    | AVar i =>
      match (sigma i) with
        | mk_typ _ t =>
          match t with
            | aexplit a' => a'
            | _ => aexplit_zero (*error!*)
          end
      end
  end.

Fixpoint eval_bexp (b: Bexp) (sigma : state) : BexpLit :=
  match b with
    | BLit b' => b'
    | BVar i =>
      match (sigma i) with
        | mk_typ _ t =>
          match t with
            | bexplit b' => b'
            | _ => bexp_error (* error! *)
          end
      end
    | BNot a => match eval_bexp a sigma with
        | mk_bexp_lit true => mk_bexp_lit false
        | mk_bexp_lit false => mk_bexp_lit true
        | _ => bexp_error
      end
    | BAnd a b => match (eval_bexp a sigma, eval_bexp b sigma) with
        | (mk_bexp_lit true, mk_bexp_lit true) => mk_bexp_lit true
        | (bexp_error, _) => bexp_error
        | (_, bexp_error) => bexp_error
        | _ => mk_bexp_lit false
      end
    | BOr a b => match (eval_bexp a sigma, eval_bexp b sigma) with
        | (mk_bexp_lit false, mk_bexp_lit false) => mk_bexp_lit false
        | (bexp_error, _) => bexp_error
        | (_, bexp_error) => bexp_error
        | _ => mk_bexp_lit true
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

(** Inner function for eval_command. It uses a counter n to avoid infinite loops *)
Fixpoint eval_command_inner (cmd: Command) (sigma: state) (n: nat): state :=
  match n with
    | O => sigma
    | S n' => 
      match cmd with
        | CWhile b c =>
          if eval_bexp b sigma then
            let s' := eval_command_inner c sigma n' in
              eval_command_inner cmd s' n'
          else sigma
        | CRepeat a c =>
          let a' := eval_aexp a sigma in
            match a' with
              | mk_aexp_lit (Qmake 0 1) => sigma
              | mk_aexp_lit (Qmake z 1) =>
                let s' := eval_command_inner c sigma n' in
                  eval_command_inner (CRepeat (ALit (mk_aexp_lit (Qmake (z-1) 1))) c) s' n'
              | _ => sigma (* TODO: Error, repeating on a decimal *)
            end
        | CSet i t => update sigma i t
        | CLet i t => update sigma i t
        | CSkip => sigma
        | CPrint i => sigma (* TODO *)
        | CIf b c1 c2 =>
          match eval_bexp b sigma with
            | mk_bexp_lit true => eval_command_inner c1 sigma n'
            | mk_bexp_lit false => eval_command_inner c2 sigma n'
            | bexp_error => sigma (* TODO: Error, if on an error *)
          end
        | CMatch i t_lst c_lst =>
          match t_lst with
            | nil => sigma
            | cons t_h t_t =>
              match c_lst with
                | nil => sigma
                | cons c_h c_t => 
                  match t_h with
                    | mk_typ _ ti =>
                      match ti with
                        (* Add new types here *)
                        | aexplit (mk_aexp_lit qmatch) =>
                          (* TODO: We need to check if the ident is that type first *)
                          let ival := eval_aexp (AVar i) sigma in
                            match ival with
                              | mk_aexp_lit qval =>
                                if Qeq_bool qval qmatch then
                                  eval_command_inner c_h sigma n'
                                else
                                  eval_command_inner (CMatch i t_t c_t) sigma n'
      	       	       	      | aexp_error => eval_command_inner (CMatch i t_t c_t) sigma n' (* Then there is no match here *)
                            end
                        | aexplit aexp_error => sigma (* TODO: Error, matching on an error *)
                        | bexplit bexp_error => sigma (* TODO: Error, matching on an error *)
                        | bexplit lit =>
                          (* TODO: We need to check if the ident is that type first *)
                          let ival := eval_bexp (BVar i) sigma in
                            match ival, lit with
                              | mk_bexp_lit true, mk_bexp_lit true => eval_command_inner c_h sigma n'
                              | mk_bexp_lit false, mk_bexp_lit false => eval_command_inner c_h sigma n'
                              | bexp_error, _ => sigma (* TODO: error *)
                              | _, bexp_error => sigma (* TODO: error *)
                              | _, _ => eval_command_inner (CMatch i t_t c_t) sigma n'
                            end
                      end
                  end
              end
          end
        | CSeq c1 c2 => eval_command_inner c2 (eval_command_inner c1 sigma n') n'
      end
  end.

Definition eval_command (cmd: Command) (sigma: state): state :=
  eval_command_inner cmd sigma 5000.

End Eval.

Export Eval.
