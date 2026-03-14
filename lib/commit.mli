(** Commit object: a snapshot of database state with history.

    A commit points to a database state (via hash), records parent
    commits, and carries a message. Stored as a content-addressed
    blob in the store.

    Binary format:
    [state_hash: 32B] [parent_count: 2B BE] [parent_hashes: N*32B]
    [message_len: 2B BE] [message] *)

type t = {
  state : Hash.t;
  parents : Hash.t list;
  message : string;
}

val encode : t -> string
val decode : string -> t
