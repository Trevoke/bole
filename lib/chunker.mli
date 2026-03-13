(** Content-defined chunk boundary detection with dynamic probability.

    Determines where to split sorted key sequences into chunks.
    Uses BLAKE2s hash of each key and a threshold that increases
    boundary probability as the chunk grows past the target size.

    Below [target_size / 4]: boundary probability is 0.
    At [target_size]: boundary probability per key is ~1/target.
    Above [2 * target_size]: boundary probability approaches certainty. *)

type t

(** [create ~target_size] creates a chunker targeting approximately
    [target_size] entries per chunk. *)
val create : target_size:int -> t

(** [feed t ~key] feeds a key and returns [true] if a chunk boundary
    should be placed after this key. *)
val feed : t -> key:string -> bool

(** [reset t] resets the chunker for a new chunk. Call after each boundary. *)
val reset : t -> unit

(** [count t] returns the number of items fed since the last reset. *)
val count : t -> int
