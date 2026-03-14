(** Repository: bridge between .bole/ directory and Db.t.

    Handles creating, loading, and saving the on-disk repository
    state including HEAD, branch refs, and the object store. *)

val init : string -> unit
(** [init path] creates a .bole/ directory structure at [path].
    Creates .bole/HEAD, .bole/refs/heads/, .bole/objects/. *)

val load : string -> Db.t
(** [load path] loads a Db.t from the .bole/ directory at [path].
    Reads HEAD for current branch, refs/heads/ for branch pointers,
    and opens the object store. *)

val save : string -> Db.t -> unit
(** [save path db] persists HEAD and branch refs to the .bole/ directory.
    Object store is already persisted via Store.put. *)

val find_root : unit -> string option
(** [find_root ()] searches upward from the current directory for a
    directory containing .bole/. Returns [Some path] or [None]. *)
