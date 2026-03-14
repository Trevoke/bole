(** Diffable, mergeable database backed by prolly trees.

    Each database holds named tables (prolly trees keyed by primary key),
    supports commits, branches, diffs, and three-way merge. *)

type t

type conflict = {
  table : string;
  key : string;
  base : string option;
  ours : string option;
  theirs : string option;
}

type merge_result = {
  db : t;
  conflicts : conflict list;
}

val create : unit -> t
val store : t -> Store.t
val current_branch : t -> string
val branch_heads : t -> (string * Hash.t) list
val working_tables : t -> (string * Hash.t) list
val of_parts :
  store:Store.t ->
  branches:(string * Hash.t) list ->
  current_branch:string ->
  head_commit:Hash.t option ->
  ?working_tables:(string * Hash.t) list ->
  unit ->
  t
val put : t -> table:string -> key:string -> value:string -> t
val delete : t -> table:string -> key:string -> t
val find : t -> table:string -> key:string -> string option
val commit : t -> message:string -> Hash.t * t
val checkout : t -> Hash.t -> t
val parents : t -> Hash.t -> Hash.t list
val branch : t -> name:string -> t
val switch : t -> name:string -> t
val diff : t -> from:Hash.t -> to_:Hash.t -> table:string -> Diff.entry Seq.t
val range : t -> table:string -> (string * string) Seq.t
val merge : t -> ours:string -> theirs:string -> merge_result
