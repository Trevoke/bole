(** Content hash using BLAKE2s-256. *)

(** The type of a 32-byte hash digest. Opaque. *)
type t

(** [hash s] computes the BLAKE2s-256 digest of [s]. *)
val hash : string -> t

(** Constant-time equality. *)
val equal : t -> t -> bool

(** Fast total ordering, suitable for use as map/set keys. NOT constant-time. *)
val compare : t -> t -> int

(** Raw 32-byte binary digest for serialization. *)
val to_raw_string : t -> string

(** Construct from raw 32-byte string. Raises [Invalid_argument] if wrong length. *)
val of_raw_string : string -> t

(** Hex-encoded digest for display/debugging. *)
val to_hex : t -> string

(** Construct from hex-encoded string. Raises [Invalid_argument] if malformed. *)
val of_hex : string -> t

(** Digest size in bytes. Always 32. *)
val hash_size : int
