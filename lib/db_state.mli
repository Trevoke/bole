(** Database state: a map of table names to their prolly tree root hashes.

    Serialized as a content-addressed blob in the store. Entries are
    sorted by name for deterministic hashing.

    Binary format: [entry_count: 2B BE] then per entry:
    [name_len: 2B BE] [name] [root_hash: 32B] *)

type table_entry = { name : string; root : Hash.t }

type t = table_entry list

val encode : t -> string
val decode : string -> t
