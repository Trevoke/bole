(** Content-addressed chunk store with optional file persistence. *)

(** The store type. *)
type t

(** [create ?path ()] returns a fresh store.
    If [path] is provided, objects are persisted to that directory
    using git-style 2-char prefix subdirectories (e.g. ab/cdef...).
    If [path] is omitted, the store is purely in-memory. *)
val create : ?path:string -> unit -> t

(** [put store data] stores [data] and returns its content hash.
    Idempotent: storing the same data twice is a no-op. *)
val put : t -> string -> Hash.t

(** [get store h] retrieves the data stored under [h].
    @raise Not_found if [h] is not in the store. *)
val get : t -> Hash.t -> string

(** [mem store h] returns [true] if [h] is in the store. *)
val mem : t -> Hash.t -> bool

(** [get_count store] returns the number of [get] calls since creation
    or the last [reset_stats]. *)
val get_count : t -> int

(** [put_count store] returns the number of [put] calls since creation
    or the last [reset_stats]. *)
val put_count : t -> int

(** [reset_stats store] resets [get_count] and [put_count] to zero. *)
val reset_stats : t -> unit
