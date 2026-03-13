(** Content-defined chunk boundary detection with dynamic probability.

    Determines where to split sorted key sequences into chunks.
    Uses BLAKE2s hash of each key and a quadratic ramp that increases
    boundary probability as the chunk grows.

    Below [target_size / 4]: boundary probability is 0.
    At [target_size]: per-key boundary probability is ~56%.
    At [2 * target_size]: forced boundary.

    Note: mean chunk size is approximately 0.5x-0.8x of [target_size]
    due to cumulative probability. Callers should set [target_size]
    higher than their desired mean to compensate. *)

type t

(** [create ~target_size ~level] creates a chunker targeting approximately
    [target_size] entries per chunk. The [level] parameter salts boundary
    detection so that the same key sequence produces different boundaries
    at different tree levels. *)
val create : target_size:int -> level:int -> t

(** [feed t ~key] feeds a key and returns [true] if a chunk boundary
    should be placed after this key. *)
val feed : t -> key:string -> bool

(** [reset t] resets the chunker for a new chunk. Call after each boundary. *)
val reset : t -> unit

(** [count t] returns the number of items fed since the last reset. *)
val count : t -> int
