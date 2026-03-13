(** Binary serialization for prolly tree node chunks.

    Leaf chunks store (key, value) entries.
    Internal chunks store (key, child_hash) entries.

    Binary format:
    - Byte 0: node type (0x00 = leaf, 0x01 = internal)
    - Remaining bytes: entries encoded sequentially until EOF
    - Leaf entry: [key_len: 2B BE] [key] [val_len: 2B BE] [value]
    - Internal entry: [key_len: 2B BE] [key] [child_hash: 32B] *)

type leaf_entry = { key : string; value : string }

type internal_entry = { key : string; child : Hash.t }

type t =
  | Leaf of leaf_entry list
  | Internal of internal_entry list

(** [encode chunk] serializes a chunk to its binary representation. *)
val encode : t -> string

(** [decode data] deserializes a chunk from its binary representation.
    @raise Invalid_argument if the data is malformed or truncated. *)
val decode : string -> t
