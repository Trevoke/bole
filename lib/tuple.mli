(** Order-preserving tuple encoding.

    Encodes typed value lists into bytes such that byte comparison
    (String.compare) gives the correct sort order. Used for prolly
    tree keys that need numeric or composite ordering.

    Encoding format: each value is type-tagged.
    - Int64 (tag 0x01): 8 bytes big-endian with sign-bit flip
    - String (tag 0x02): byte-stuffed (0x00 -> 0x00 0xFF) + 0x00 0x00 terminator
    - Uuid (tag 0x03): 16 raw bytes (UUIDv7 sorts chronologically)
    - Bool (tag 0x04): 1 byte (0x00 false, 0x01 true)
    - Float (tag 0x05): 8 bytes IEEE 754 with sign manipulation
    - Timestamp (tag 0x06): 8 bytes big-endian with sign-bit flip
    - Blob (tag 0x07): byte-stuffed + 0x00 0x00 terminator *)

type value =
  | Int64 of int64
  | String of string
  | Uuid of Uuid.t
  | Bool of bool
  | Float of float
  | Timestamp of int64
  | Blob of string

type t = value list

val encode : t -> string
val decode : string -> t
