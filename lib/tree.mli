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
