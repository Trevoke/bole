(** Compute differences between two prolly trees.

    [diff store ~from ~to_] returns a lazy sequence of differences
    between the tree rooted at [from] (old) and the tree rooted at
    [to_] (new). Entries are emitted in key order.

    - [Removed(key, value)]: present in [from] but absent in [to_]
    - [Added(key, value)]: present in [to_] but absent in [from]
    - [Modified(key, old_value, new_value)]: key exists in both with different values

    Skips shared subtrees by comparing hashes, achieving O(d log n)
    where d is the number of differing entries. *)

type entry =
  | Added of string * string
  | Removed of string * string
  | Modified of string * string * string

val diff : Store.t -> from:Hash.t -> to_:Hash.t -> entry Seq.t
