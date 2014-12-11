Require Export Evals.

Lemma substring_self : forall (s: string),
  substring 0 (length s) s = s.
Proof.
  intros.
  induction s.
  compute; reflexivity.
  simpl.
  rewrite -> IHs.
  reflexivity.
Qed.

(** We do weird things for string equality because there's no built
 in function for it. Let's prove that it works! *)
Theorem matching_strings : forall (s1 s2: string), s1 = s2 ->
  sexplit_is_equal (mk_sexp_lit s1) (mk_sexp_lit s2) = mk_bexp_lit true.
Proof.
  intros.
  simpl.
  assert (prefix s1 s2 = true).
    rewrite prefix_correct.
    rewrite -> H.
    apply substring_self.
  assert ((beq_nat (length s1) (length s2)) = true).
    rewrite -> H.
    symmetry.
    apply beq_nat_refl.
  rewrite H0.
  rewrite H1.
  compute.
  reflexivity.
Qed.