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
  | CMatch: Exp -> Matchbody -> Command
  | CSeq: Command -> Command -> Command (* for sequential commands *)
with Matchbody :=
  | MBNone: Matchbody
  | MBSome: Exp -> Command -> Matchbody -> Matchbody.

End Commands.

Export Commands.