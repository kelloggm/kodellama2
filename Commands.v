(** Commands *)

Require Export Aexp.
Require Export Bexp.
Require Export Typ.

Module Commands.

Inductive Command :=
  | CWhile: Bexp -> Command -> Command
  | CRepeat: Aexp -> Command -> Command
  | CSet: ident -> Exp -> Command
  | CLet: ident -> Exp -> Command
  | CSkip: Command
  | CPrint: ident -> Command
  | CIf: Bexp -> Command -> Command -> Command
  | CMatch: ident -> list typ -> list Command -> Command
  | CSeq: Command -> Command -> Command (* for sequential commands *).

End Commands.

Export Commands.