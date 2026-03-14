# Step 6 Design: Point Lookup and Range Scan

## What we're building

Two new functions in the `Tree` module: `find` for point lookup and `range` for range scanning. Both traverse the prolly tree from root to leaves using `Store.get` to load chunks, with linear scan within chunks.

## Design decisions

- **Single module:** Both functions live in `Tree` — no separate `Cursor` module. The range scan's path state is internal to the `Seq.t` closure.
- **Return types:** `find` returns `string option` (missing key is normal, not exceptional). `range` returns `(string * string) Seq.t` (lazy, streams on demand).
- **Range bounds:** `?start_key:string -> ?end_key:string`, both optional. End is exclusive. Omit either for an unbounded side. Both omitted = full scan.
- **In-chunk search:** Linear scan. Chunks are typically 10-50 entries; linear scan is simpler and likely faster than array conversion + binary search at these sizes. The dominant cost is `Store.get`, not in-chunk search.
- **Descent:** Path-based. Record the descent path as a list of frames so range scan can advance to sibling chunks without re-descending from the root.

## Interface additions

```ocaml
(** tree.mli — additions *)

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
```

## Point lookup algorithm

```
find(store, root_hash, key):
  hash = root_hash
  loop:
    data = Store.get store hash
    chunk = Chunk.decode data

    match chunk:
    | Leaf entries ->
        linear scan entries for entry.key = key
        return Some entry.value if found, None otherwise

    | Internal entries ->
        find the first entry where entry.key >= key
        if no such entry exists, return None
          (key is greater than all keys in tree)
        hash = entry.child
        continue loop
```

Internal entry keys are the **last key** of the child subtree. So the first entry with `key >= target` is the child whose range covers the target.

## Range scan algorithm

### Path representation

```ocaml
type path_frame = {
  entries : Chunk.internal_entry array;
  index : int;
}
```

The path is a `path_frame list` — head is closest to the leaf level, last element is closest to the root. Each frame records the internal node's entries (as an array for index access) and which child we descended into.

### Algorithm

```
range(store, root, ?start_key, ?end_key):

  1. Descend from root to the relevant leaf:
     - If start_key is Some:
         at each internal node, find first entry.key >= start_key
         record (entries, index) on the path
         descend into that child
     - If start_key is None:
         at each internal node, take the first child (index 0)
         record (entries, 0) on the path
         descend

  2. At the leaf, find the starting position:
     - If start_key is Some: first entry with key >= start_key
     - If start_key is None: first entry

  3. Build a Seq.t that yields entries:
     - Yield entries from current leaf starting at position
     - Stop yielding if key >= end_key (when end_key is Some)
     - When leaf is exhausted, advance via path:
         walk up path frames looking for a frame where
           index + 1 < Array.length entries
         if found:
           advance to index + 1
           descend to leftmost leaf from that child
           continue yielding from new leaf
         if not found:
           sequence ends (no more entries in tree)
```

### Edge cases

- **Empty tree:** Decode root → empty Leaf → empty Seq.
- **start_key beyond all keys:** Descent reaches a leaf with no matching entries → empty Seq.
- **end_key before all keys:** First entry already >= end_key → empty Seq.
- **Single chunk tree:** No path frames. Yield matching entries from the single leaf.

## Tests

### Unit tests

- **`find` existing key:** 10-entry single-chunk tree, look up a key that exists → `Some value`.
- **`find` missing key:** Same tree, look up non-existent key → `None`.
- **`find` in multi-chunk tree:** 200 entries, `target_size:20`, look up keys at various positions (first, middle, last) → correct values.
- **`find` in empty tree:** → `None`.
- **`range` full scan:** Build tree, `range store root` with no bounds, collect all → equals original sorted input.
- **`range` with `start_key`:** Only entries with key >= start_key returned.
- **`range` with `end_key`:** Only entries with key < end_key returned.
- **`range` with both bounds:** Entries in [start, end) returned.
- **`range` empty result:** Bounds that match nothing → empty Seq.
- **`range` on empty tree:** → empty Seq.

### Property tests (QCheck)

- **`find` round-trip:** Build from random sorted pairs. Every inserted key returns `Some` with correct value. Random keys not in the set return `None`.
- **`range` full scan equals input:** Build from random sorted pairs. `range` with no bounds equals original list.
- **`range` bounds filter:** Build from random pairs. Pick random start/end. Verify `range ~start_key ~end_key` equals filtering the original list with `key >= start && key < end`.
