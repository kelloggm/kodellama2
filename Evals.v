(* Contains the code that actually evaluates expressions and commands *)

Require Export Typ.
Require Export Sigma.
Require Export Aexp.
Require Export Exp.
Require Export Commands.

Module Evals.

Fixpoint eval_aexp (a : Aexp) (sigma : state) :=
  match a with
    | ALit a' => a'
    | APlus b c => plus_aexplit (eval_aexp b sigma) (eval_aexp c sigma)
    | AMinus b c => minus_aexplit (eval_aexp b sigma) (eval_aexp c sigma)
    | AMult b c => mult_aexplit (eval_aexp b sigma) (eval_aexp c sigma)
    | ADiv b c => div_aexplit (eval_aexp b sigma) (eval_aexp c sigma)
    | AExp b c => exp_aexplit (eval_aexp b sigma) (eval_aexp c sigma)
    | AMod b c => mod_aexplit (eval_aexp b sigma) (eval_aexp c sigma)
    | ANeg b => neg_aexplit (eval_aexp b sigma)
    | AVar i =>
      match (sigma i) with
        | mk_typ _ t =>
          match t with
            | mk_explit_from_aexp a' => a'
            | _ => aexp_error (*error!*)
          end
      end
  end.

Definition aexplit_is_equal (e1 e2: AexpLit): BexpLit :=
  match e1, e2 with
    | aexp_error, _ => bexp_error
    | _, aexp_error => bexp_error
    | mk_aexp_lit q1, mk_aexp_lit q2 =>
      if Qeq_bool q1 q2 then mk_bexp_lit true else mk_bexp_lit false
  end.

Definition aexp_is_equal (e1 e2: Aexp) (sigma: state): BexpLit :=
  aexplit_is_equal (eval_aexp e1 sigma) (eval_aexp e2 sigma).

Fixpoint eval_sexp (e: Sexp) (sigma: state): SexpLit :=
  match e with
    | SLit s => s
    | SConcat s1 s2 =>
      let s1eval := eval_sexp s1 sigma in
      let s2eval := eval_sexp s2 sigma in
        match s1eval, s2eval with
          | sexp_error, _ => sexp_error
          | _, sexp_error => sexp_error
          | mk_sexp_lit str1, mk_sexp_lit str2 =>
            mk_sexp_lit (append str1 str2)
        end
    | SVar i => 
      match (sigma i) with
        | mk_typ _ t =>
          match t with
            | mk_explit_from_sexp s' => s'
            | _ => sexp_error (* error! *)
          end
      end
  end.

Definition sexplit_is_equal (s1 s2: SexpLit): BexpLit :=
  match s1, s2 with
    | mk_sexp_lit s1', mk_sexp_lit s2' => mk_bexp_lit (andb (prefix s1' s2') (EqNat.beq_nat (length s1') (length s2')))
    | _, _ => mk_bexp_lit false
  end.

Definition sexp_is_equal (s1 s2: Sexp) (sigma: state): BexpLit :=
  sexplit_is_equal (eval_sexp s1 sigma) (eval_sexp s2 sigma).

Definition uexp_is_equal (u1 u2: Uexp): BexpLit :=
  match u1, u2 with
    | Uexpid (mk_id s1), Uexpid (mk_id s2) => mk_bexp_lit (andb (prefix s1 s2) (EqNat.beq_nat (length s1) (length s2)))
    | _, _ => mk_bexp_lit false (* TODO: Some plus statements could be equal *)
  end.

Definition bexplit_is_equal (e1 e2: BexpLit): BexpLit :=
  match e1, e2 with
    | bexp_error, _ => bexp_error
    | _, bexp_error => bexp_error
    | mk_bexp_lit b1, mk_bexp_lit b2 => mk_bexp_lit (eqb b1 b2)
  end.

Fixpoint eval_bexp (b: Bexp) (sigma : state) (n: nat) : BexpLit :=
  match n with
    | O => bexp_error (* Too many computations *)
    | S n' =>
      match b with
        | BLit b' => b'
        | BVar i =>
          match (sigma i) with
            | mk_typ _ t =>
              match t with
                | mk_explit_from_bexp b' => b'
                | _ => bexp_error (* error! *)
              end
          end
        | BNot a => match eval_bexp a sigma n' with
            | mk_bexp_lit true => mk_bexp_lit false
            | mk_bexp_lit false => mk_bexp_lit true
            | _ => bexp_error
          end
        | BAnd a b => match (eval_bexp a sigma n', eval_bexp b sigma n') with
            | (mk_bexp_lit true, mk_bexp_lit true) => mk_bexp_lit true
            | (bexp_error, _) => bexp_error
            | (_, bexp_error) => bexp_error
            | _ => mk_bexp_lit false
          end
        | BOr a b => match (eval_bexp a sigma n', eval_bexp b sigma n') with
            | (mk_bexp_lit false, mk_bexp_lit false) => mk_bexp_lit false
            | (bexp_error, _) => bexp_error
            | (_, bexp_error) => bexp_error
            | _ => mk_bexp_lit true
          end
        | BEq e1 e2 => exp_is_equal e1 e2 sigma n'
        | BLt e1 e2 =>
          let eval1 := eval_aexp e1 sigma in
          let eval2 := eval_aexp e2 sigma in
            match eval1, eval2 with
              | aexp_error, _ => bexp_error
              | _, aexp_error => bexp_error
              | mk_aexp_lit q1, mk_aexp_lit q2 =>
                mk_bexp_lit (andb (Qle_bool q1 q2) (negb (Qeq_bool q1 q2)))
            end
        | BGt e1 e2 =>
          let eval1 := eval_aexp e1 sigma in
          let eval2 := eval_aexp e2 sigma in
            match eval1, eval2 with
              | aexp_error, _ => bexp_error
              | _, aexp_error => bexp_error
              | mk_aexp_lit q1, mk_aexp_lit q2 =>
                mk_bexp_lit (andb (Qle_bool q2 q1) (negb (Qeq_bool q1 q2)))
            end
        | BLe e1 e2 =>
          let eval1 := eval_aexp e1 sigma in
          let eval2 := eval_aexp e2 sigma in
            match eval1, eval2 with
              | aexp_error, _ => bexp_error
              | _, aexp_error => bexp_error
              | mk_aexp_lit q1, mk_aexp_lit q2 =>
                mk_bexp_lit (Qle_bool q1 q2)
            end
        | BGe e1 e2 =>
          let eval1 := eval_aexp e1 sigma in
          let eval2 := eval_aexp e2 sigma in
            match eval1, eval2 with
              | aexp_error, _ => bexp_error
              | _, aexp_error => bexp_error
              | mk_aexp_lit q1, mk_aexp_lit q2 =>
                mk_bexp_lit (Qle_bool q2 q1)
            end
      end
  end

with exp_is_equal (e1 e2: Exp) (sigma: state) (n: nat): BexpLit :=
  match n, e1, e2 with
    | O, _, _ => bexp_error
    | S n', EAexp a1, EAexp a2 => aexp_is_equal a1 a2 sigma
    | S n', EBexp b1, EBexp b2 => bexp_is_equal b1 b2 sigma n'
    | S n', ESexp s1, ESexp s2 => sexp_is_equal s1 s2 sigma
    | S n', EUexp u1, EUexp u2 => uexp_is_equal u1 u2
    | S n', EUexp (Uexpid i), EAexp a2 =>
      match (sigma i) with
        | mk_typ  _ (mk_explit_from_aexp alit) => aexplit_is_equal alit (eval_aexp a2 sigma)
        | _ => mk_bexp_lit false
      end
    | S n', EAexp a2, EUexp (Uexpid i) =>
      match (sigma i) with
        | mk_typ  _ (mk_explit_from_aexp alit) => aexplit_is_equal alit (eval_aexp a2 sigma)
        | _ => mk_bexp_lit false
      end
    | S n', EUexp (Uexpid i), EBexp b2 =>
      match (sigma i) with
        | mk_typ  _ (mk_explit_from_bexp blit) => bexplit_is_equal blit (eval_bexp b2 sigma n')
        | _ => mk_bexp_lit false
      end
    | S n', EBexp b2, EUexp (Uexpid i) =>
      match (sigma i) with
        | mk_typ  _ (mk_explit_from_bexp blit) => bexplit_is_equal blit (eval_bexp b2 sigma n')
        | _ => mk_bexp_lit false
      end
    | S n', EUexp (Uexpid i), ESexp s2 =>
      match (sigma i) with
        | mk_typ  _ (mk_explit_from_sexp slit) => sexplit_is_equal slit (eval_sexp s2 sigma)
        | _ => mk_bexp_lit false
      end
    | S n', ESexp s2, EUexp (Uexpid i) =>
      match (sigma i) with
        | mk_typ  _ (mk_explit_from_sexp slit) => sexplit_is_equal slit (eval_sexp s2 sigma)
        | _ => mk_bexp_lit false
      end
    | _, _, _ => mk_bexp_lit false (* TODO? This gives false even if one argument evaluates to an error *)
  end

with bexp_is_equal (e1 e2: Bexp) (sigma: state) (n: nat): BexpLit :=
  match n with
    | O => bexp_error
    | S n' => bexplit_is_equal (eval_bexp e1 sigma n') (eval_bexp e2 sigma n')
  end.

Fixpoint eval_exp (e: Exp) (sigma: state) :=
  match e with
    | EAexp a => mk_explit_from_aexp (eval_aexp a sigma)
    | EBexp b => mk_explit_from_bexp (eval_bexp b sigma 5)
    | ESexp s => mk_explit_from_sexp (eval_sexp s sigma)
    | EUexp u => eval_uexp u sigma
  end

with eval_uexp (e: Uexp) (sigma: state) :=
  match e with
    | Uexpid i =>
      match (sigma i) with
        | mk_typ _ t => t
      end
    | Uexpplus e1 e2 =>
      let e1eval := eval_uexp e1 sigma in
      let e2eval := eval_exp e2 sigma in
        match e1eval with
          | mk_explit_from_aexp alit1 => 
            match e2eval with
              | mk_explit_from_aexp alit2 => mk_explit_from_aexp (plus_aexplit alit1 alit2)
              | mk_explit_from_bexp _ => mk_explit_from_bexp bexp_error
              | mk_explit_from_sexp _ => mk_explit_from_sexp sexp_error
              | exp_error => exp_error
            end
          | mk_explit_from_bexp _ => mk_explit_from_bexp bexp_error
          | mk_explit_from_sexp slit1 =>
            match e2eval with
              | mk_explit_from_aexp _ => mk_explit_from_aexp aexp_error
              | mk_explit_from_bexp _ => mk_explit_from_bexp bexp_error
              | mk_explit_from_sexp slit2 => mk_explit_from_sexp (eval_sexp (SConcat (SLit slit1) (SLit slit2)) sigma)
              | exp_error => exp_error
            end
          | exp_error => exp_error
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
    | O => sigma (* TODO: Error, infinite loop or large program *)
    | S n' => 
      match cmd with
        | CWhile b c =>
          let blit := eval_bexp b sigma 5 in
          match blit with
            | mk_bexp_lit true => 
              let s' := eval_command_inner c sigma n' in
              eval_command_inner cmd s' n'
            | _ => sigma
          end
        | CRepeat a c =>
          let a' := eval_aexp a sigma in
            match a' with
              | mk_aexp_lit (Qmake 0 1) => sigma
              | mk_aexp_lit (Qmake z 1) =>
                let s' := eval_command_inner c sigma n' in
                  eval_command_inner (CRepeat (ALit (mk_aexp_lit (Qmake (z-1) 1))) c) s' n'
              | _ => sigma (* TODO: Error, repeating on a decimal *)
            end
        | CSet i e => update sigma i (mk_typ false (eval_exp e sigma))
        | CLet i e => update sigma i (mk_typ true (eval_exp e sigma))
        | CSkip => sigma
        | CPrint exp => initial_state (* Trash so the exporter doesnt optimize it out *)
        | CIf b c1 c2 =>
          match eval_bexp b sigma 5 with
            | mk_bexp_lit true => eval_command_inner c1 sigma n'
            | mk_bexp_lit false => eval_command_inner c2 sigma n'
            | bexp_error => sigma (* TODO: Error, if on an error *)
          end
        | CMatch exp Matchbody =>
          match Matchbody with
            | MBNone => sigma
            | MBSome m_exp m_cmd mb =>
              match exp_is_equal exp m_exp sigma 5 with
                | mk_bexp_lit true => eval_command_inner m_cmd sigma n'
                | mk_bexp_lit false => eval_command_inner (CMatch exp mb) sigma n'
                | bexp_error => sigma (* TODO: Error evaluating conditions *)
              end
          end
        | CSeq c1 c2 => eval_command_inner c2 (eval_command_inner c1 sigma n') n'
      end
  end.

Definition eval_command (cmd: Command) (sigma: state): state :=
  eval_command_inner cmd sigma 5.

End Evals.

Export Evals.
