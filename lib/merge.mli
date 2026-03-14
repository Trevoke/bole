(** Three-way merge of two diff streams.

    Given two diff streams (ancestor->ours and ancestor->theirs),
    merge-joins them by key to produce non-conflicting changes
    and a list of conflicts.

    Changes are relative to the ours tree — only theirs-side
    non-conflicting changes appear in the changes list. *)

type change =
  | Put of string * string
  | Delete of string

type conflict = {
  key : string;
  base : string option;
  ours : string option;
  theirs : string option;
}

type result = {
  changes : change list;
  conflicts : conflict list;
}

val three_way : ours:Diff.entry Seq.t -> theirs:Diff.entry Seq.t -> result
