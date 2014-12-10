(* @ Owner Scelerus *)

open Printf

let main () = 
  printf "Enter a Kodellama command:\n" ; 
  flush stdout ;
  let stIn = ref "" in
  try
	while true do
	      stIn := (!stIn ^ (input_line stdin))
	done; 
  with End_of_file -> () ;
  let lexbuf = Lexing.from_string !stIn in
  let kdl_command = (Parse.com Lex.initial lexbuf) in
  let sigma_0 = Sigma.Sigma.initial_state in 
  let sigma_n = Evals.Evals.eval_command kdl_command sigma_0 in
  ignore (sigma_n) ; 
  print_endline "" ;
  exit 0 
;;
main () ;;
