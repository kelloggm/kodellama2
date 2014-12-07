(* @Owner MattIrv *)

Require Export QArith.
Require Export String.

Module BinStringToQ.

Inductive sign :=
  | sZero: sign
  | sPos: sign
  | sNeg: sign.

Local Open Scope char_scope.

Fixpoint bin_str_to_pos (str: string) (cp: positive) (start: bool) :=
  match start with
    | true =>
      match str with
        | EmptyString => None
        | String char s' =>
          match char with
            | "0" => bin_str_to_pos s' xH true
            | "1" => bin_str_to_pos s' xH false
            | _ => None
          end
      end
    | false =>
      match str with
        | EmptyString => Some cp
        | String char s' =>
          match char with
            | "0" => bin_str_to_pos s' (xO cp) false
            | "1" => bin_str_to_pos s' (xI cp) false
            | _ => None
          end
      end
  end.

Local Close Scope char_scope.

Definition numstr_to_z (numStr: string) (s: sign) :=
  match s with
    | sZero => Some Z0
    | sPos => let somepos := bin_str_to_pos numStr xH true in
      match somepos with
        | Some p => Some (Zpos p)
        | None => None
      end
    | sNeg => let somepos := bin_str_to_pos numStr xH true in
      match somepos with
        | Some p => Some (Zneg p)
        | None => None
      end
  end.

Definition bin_str_to_q (numStr denStr: string) (s: sign) :=
  let num := numstr_to_z numStr s in
  let denom := bin_str_to_pos denStr xH true in
    match num, denom with
      | None, _ => None
      | _, None => None
      | Some z, Some p => Some (Qmake z p)
    end.

Local Open Scope char_scope.

Fixpoint hex_str_to_bin_str (hexstr: string) :=
  match hexstr with
    | EmptyString => EmptyString
    | String char s' =>
      match char with
        | "0" => "0000" ++ hex_str_to_bin_str s'
        | "1" => "0001" ++ hex_str_to_bin_str s'
        | "2" => "0010" ++ hex_str_to_bin_str s'
        | "3" => "0011" ++ hex_str_to_bin_str s'
        | "4" => "0100" ++ hex_str_to_bin_str s'
        | "5" => "0101" ++ hex_str_to_bin_str s'
        | "6" => "0110" ++ hex_str_to_bin_str s'
        | "7" => "0111" ++ hex_str_to_bin_str s'
        | "8" => "1000" ++ hex_str_to_bin_str s'
        | "9" => "1001" ++ hex_str_to_bin_str s'
        | "A" => "1010" ++ hex_str_to_bin_str s'
        | "B" => "1011" ++ hex_str_to_bin_str s'
        | "C" => "1100" ++ hex_str_to_bin_str s'
        | "D" => "1101" ++ hex_str_to_bin_str s'
        | "E" => "1110" ++ hex_str_to_bin_str s'
        | "F" => "1111" ++ hex_str_to_bin_str s'
        | _ => "ERROR"%string
      end
  end
where "s1 ++ s2" := (append s1 s2).

Local Close Scope char_scope.

Definition hex_str_to_q (numStr denStr: string) (s: sign) :=
  let numBin := hex_str_to_bin_str numStr in
  let denBin := hex_str_to_bin_str denStr in
  bin_str_to_q numBin denBin s.

Example e1 := bin_str_to_q "0100101" "101010" sPos.
Example e2 := bin_str_to_q "0" "1" sPos.
Example e3 := bin_str_to_q "1" "10" sNeg.
Example h1 := hex_str_to_bin_str "2A".

End BinStringToQ.

Export BinStringToQ.