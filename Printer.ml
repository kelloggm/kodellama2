let rec implode s = match s with
	| [] -> ""
	| h :: t -> String.concat "" [(String.make 1 h) ; implode t]

(* f is the eval function. Needs to be passed since Evals module needs this module to be compiled first *)
let print exp sigma f = match f exp sigma with
    | Exp.Exp.Coq_exp_error -> Printf.printf "%s" "Error evaluating expression.\n"
    | Exp.Exp.Coq_mk_explit_from_aexp alit -> (match alit with
    	| Exp.Exp.Coq_aexp_error -> Printf.printf "%s" "Error evaluating arithmetic expression.\n"
    	| Exp.Exp.Coq_mk_aexp_lit { QArith_base.coq_Qnum = num; QArith_base.coq_Qden = den } -> Printf.printf "%s" "Printing arithmetic expression.\n")
    | Exp.Exp.Coq_mk_explit_from_bexp blit -> (match blit with
    	| Exp.Exp.Coq_bexp_error -> Printf.printf "%s" "Error evaluating boolean expression.\n"
    	| Exp.Exp.Coq_mk_bexp_lit b -> let _ = Printf.printf "%B" b in Printf.printf "%s" "\n")
    | Exp.Exp.Coq_mk_explit_from_sexp slit -> (match slit with
    	| Exp.Exp.Coq_sexp_error -> Printf.printf "%s" "Error evaluating string expression.\n"
    	| Exp.Exp.Coq_mk_sexp_lit s -> let _ = Printf.printf "%s" (implode s) in Printf.printf "%s" "\n")