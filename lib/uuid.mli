(** UUIDv7: time-ordered universally unique identifiers.

    UUIDv7 uses a 48-bit Unix timestamp (milliseconds) prefix followed
    by random bytes, giving chronological sort order under byte comparison.

    Layout (128 bits / 16 bytes):
    - Bytes 0-5: 48-bit timestamp (ms since epoch), big-endian
    - Byte 6: version (0x7X) + 4 bits random
    - Byte 7: 8 bits random
    - Byte 8: variant (0b10XX_XXXX) + 6 bits random
    - Bytes 9-15: 56 bits random *)

type t

val v7 : unit -> t
val equal : t -> t -> bool
val compare : t -> t -> int
val to_raw_string : t -> string
val of_raw_string : string -> t
val to_hex : t -> string
val of_hex : string -> t
val of_string : string -> t
(** Accepts hex with or without dashes. Raises [Invalid_argument] if invalid. *)
