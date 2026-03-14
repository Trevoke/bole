(** Order-preserving tuple encoding.

    Encodes typed value lists into bytes such that byte comparison
    (String.compare) gives the correct sort order. Used for prolly
    tree keys that need numeric or composite ordering.

    Encoding format: each value is type-tagged.
    - Int64 (tag 0x01): 8 bytes big-endian with sign-bit flip
    - String (tag 0x02): bytes + 0x00 terminator *)

type value =
  | Int64 of int64
  | String of string

type t = value list

val encode : t -> string
val decode : string -> t
