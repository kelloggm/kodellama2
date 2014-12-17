(* @ Owner Scelerus *)

open Printf

let main () = 

(* from StackOverflow - why this isn't in the standard library I will never know *)
let trim str =
  if str = "" then "" else
  let search_pos init p next =
    let rec search i =
      if p i then raise(Failure "empty") else
      match str.[i] with
      | ' ' | '\n' | '\r' | '\t' -> search (next i)
      | _ -> i
    in
    search init
  in
  let len = String.length str in
  try
    let left = search_pos 0 (fun i -> i >= len) (succ)
    and right = search_pos (len - 1) (fun i -> i < 0) (pred)
    in
    String.sub str left (right - left + 1)
  with
  | Failure "empty" -> ""
in
  printf "Enter a Kodellama command:\n" ; 
  flush stdout ;
  let stIn = ref "" in
  try
	let line = ref (input_line stdin) in      
	while true do
	      if not (trim !line = "") then
	      	 if not ((String.length (trim !line) >= 3) && (String.sub (trim !line) ((String.length (trim !line)) - 3) 3 = " do")) then
		    if not ((String.length (trim !line) >= 1) && (String.sub (trim !line) ((String.length (trim !line)) - 1) 1 = ":")) then
		       line := !line ^ ";";	         
	      stIn := (!stIn ^ !line ^ "\n");
	      line := input_line stdin;
	      while (trim !line = "") do line := input_line stdin done;
	      if ((String.length (trim !line) >= 3) && (String.sub (trim !line) 0 3 = "end")) || ((String.length (trim !line) >= 4) && (String.sub (trim !line) 0 4 = "with")) then
	      	    if (String.length !stIn >= 2) then
	      	       stIn := ((String.sub !stIn 0 ((String.length !stIn) - 2)) ^ "\n")
	done; 
  with End_of_file -> () ;

  if (String.get (trim !stIn) ((String.length (trim !stIn)) - 1) = ';') then stIn := (String.sub (trim !stIn) 0 ((String.length !stIn) - 1)); 
  
  let lexbuf = Lexing.from_string !stIn in
  let kdl_command = (Parse.com Lex.initial lexbuf) in
  let sigma_0 = Sigma.Sigma.initial_state in 
  let sigma_n = Evals.Evals.eval_command kdl_command sigma_0 in
  ignore (sigma_n) ; 
  print_endline "" ;
  exit 0 
;;
main () ;;
