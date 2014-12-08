# Owner MattIrv
# Tool for making the ocaml version accept infinite loops
# Find/replace adapted from http://stackoverflow.com/questions/39086/search-and-replace-a-line-in-a-file-in-python

from tempfile import mkstemp
from shutil import move
from os import remove, close

def replace(file_path, to_replace, replace_with):
	# Create temp file
	fh, abs_path = mkstemp()
	new_file = open(abs_path, 'w')
	old_file = open(file_path)
	for line in old_file:
		new_file.write(line.replace(to_replace, replace_with))
	# Close temp file
	new_file.close()
	close(fh)
	old_file.close()
	# Remove original file
	remove(file_path)
	# Move new file
	move(abs_path, file_path)

replace('ocaml/Evals.ml', "  | S n' ->", "  | n' ->")
replace('ocaml/Evals.ml', "open String", "open CoqString")
replace('ocaml/Ident.ml', "open String", "open CoqString")
replace('ocaml/BinStringToQ.ml', "open String", "open CoqString")
replace('ocaml/Evals.ml', "     | Commands.Commands.CPrint exp -> Sigma.Sigma.initial_state", '     | Commands.Commands.CPrint exp ->\n       (match eval_exp exp sigma with\n        | Exp.Exp.Coq_exp_error -> let () = Printf.printf "%s" "Error!" in sigma\n        | Exp.Exp.Coq_mk_explit_from_aexp alit -> let _ = Printf.printf "%s" "Printing alit" in sigma\n        | Exp.Exp.Coq_mk_explit_from_bexp blit -> let _ = Printf.printf "%s" "Printing blit" in sigma\n        | Exp.Exp.Coq_mk_explit_from_sexp slit -> let _ = Printf.printf "%s" "Printing slit" in sigma)')