(* @ Owner Scelerus *)

open Printf

let main () = 
  let silent = ref false in 
  let args = [
    "--silent", Arg.Set silent, "do not display a prompt or reminder"
  ] in 
  Arg.parse args (fun _ -> ()) "" ; 

  if not !silent then begin 
    printf "Enter a Kodellama command:\n" ;
    flush stdout ; 
  end ; 
  let lexbuf = Lexing.from_channel stdin in
  let kdl_command = Parse.com Lex.initial lexbuf in
  let sigma_0 = Sigma.Sigma.initial_state in 
  let sigma_n = Evals.Evals.eval_command kdl_command sigma_0 in
  ignore (sigma_n) ; 
  print_endline "" ; 
  exit 0 
;;
main () ;;
