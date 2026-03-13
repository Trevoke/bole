(** In-memory content-addressed chunk store. *)

(** The store type. *)
type t

(** [create ()] returns a fresh empty store. *)
val create : unit -> t

(** [put store data] stores [data] and returns its content hash.
    Idempotent: storing the same data twice is a no-op. *)
val put : t -> string -> Hash.t

(** [get store h] retrieves the data stored under [h].
    @raise Not_found if [h] is not in the store. *)
val get : t -> Hash.t -> string

(** [mem store h] returns [true] if [h] is in the store. *)
val mem : t -> Hash.t -> bool
