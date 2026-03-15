(** Diffable, mergeable database backed by prolly trees.

    Each database holds named tables with schemas. Tables are prolly
    trees keyed by primary key, with non-key columns as the value.
    Supports commits, branches, diffs, and three-way cell-level merge. *)

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

val create_table : t -> table:string -> schema:Schema.t -> t
val put_row : t -> table:string -> row:(string * Tuple.value) list -> t
val get_row : t -> table:string -> key:Tuple.t -> (string * Tuple.value) list option
val delete_row : t -> table:string -> key:Tuple.t -> t
val range_rows : t -> table:string -> (string * Tuple.value) list Seq.t

val commit : t -> message:string -> Hash.t * t
val checkout : t -> Hash.t -> t
val parents : t -> Hash.t -> Hash.t list
val branch : t -> name:string -> t
val switch : t -> name:string -> t
val diff : t -> from:Hash.t -> to_:Hash.t -> table:string -> diff_entry Seq.t
val merge : t -> ours:string -> theirs:string -> merge_result

(** Internal accessors for Repo module *)
val working_state : t -> (string * Hash.t * Hash.t) list
val of_parts :
  store:Store.t ->
  branches:(string * Hash.t) list ->
  current_branch:string ->
  head_commit:Hash.t option ->
  ?working_state:(string * Hash.t * Hash.t) list ->
  unit ->
  t
