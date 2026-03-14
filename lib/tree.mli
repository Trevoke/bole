(** Build a prolly tree from a sorted sequence of key-value pairs.

    Consumes the sequence in one pass, building leaf chunks via
    content-defined chunking, then internal node levels until
    a single root chunk remains.

    @param target_size Target entries per chunk (default 64). Actual
    mean chunk size is ~0.5-0.8x of this due to the chunker's
    quadratic ramp. See README.md for details.

    @raise Invalid_argument if keys are not in sorted order. *)
val build :
  ?target_size:int ->
  Store.t ->
  (string * string) Seq.t ->
  Hash.t

(** [find store root key] looks up [key] in the tree rooted at [root].
    Returns [Some value] if the key exists, [None] otherwise. *)
val find : Store.t -> Hash.t -> string -> string option

(** [range ?start_key ?end_key store root] returns a lazy sequence of
    all [(key, value)] pairs in the tree where
    [start_key <= key < end_key].

    Omit [start_key] to scan from the beginning.
    Omit [end_key] to scan to the end.
    Omit both for a full scan.

    The sequence streams entries on demand — each pull may trigger
    [Store.get] calls to load the next chunk. *)
val range :
  ?start_key:string ->
  ?end_key:string ->
  Store.t ->
  Hash.t ->
  (string * string) Seq.t
