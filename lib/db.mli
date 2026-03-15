(** Diffable, mergeable database backed by prolly trees.

    Each database holds named tables (prolly trees keyed by primary key),
    supports commits, branches, diffs, and three-way merge.

    Keys and values are typed tuples ([Tuple.t]). The Db layer encodes
    tuples to bytes for storage and decodes them on retrieval. *)

type t

type diff_entry =
  | Added of Tuple.t * Tuple.t
  | Removed of Tuple.t * Tuple.t
  | Modified of Tuple.t * Tuple.t * Tuple.t

type conflict = {
  table : string;
  key : Tuple.t;
  base : Tuple.t option;
  ours : Tuple.t option;
  theirs : Tuple.t option;
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
val put : t -> table:string -> key:Tuple.t -> value:Tuple.t -> t
val delete : t -> table:string -> key:Tuple.t -> t
val find : t -> table:string -> key:Tuple.t -> Tuple.t option
val range : t -> table:string -> (Tuple.t * Tuple.t) Seq.t
val commit : t -> message:string -> Hash.t * t
val checkout : t -> Hash.t -> t
val parents : t -> Hash.t -> Hash.t list
val branch : t -> name:string -> t
val switch : t -> name:string -> t
val diff : t -> from:Hash.t -> to_:Hash.t -> table:string -> diff_entry Seq.t
val merge : t -> ours:string -> theirs:string -> merge_result
