# Step 7 Design: Single-Key Mutation (Put, Delete)

## What we're building

Two new functions in the `Tree` module: `put` (insert or update a key) and `delete` (remove a key). Both produce a new root hash while structurally sharing unchanged nodes with the old tree.

## Design decisions

- **API:** `put` has upsert semantics (inserts or replaces). `delete` raises `Not_found` if the key doesn't exist. Both return `Hash.t` (the new root hash).
- **Re-chunking scope:** Only the modified leaf's entries are re-chunked. Since chunk boundaries depend only on keys (not values or history), local re-chunking produces globally correct boundaries — identical to a full rebuild with the same data.
- **No neighbor loading:** Adjacent chunks are never touched. Their boundaries are determined by their own keys and are unaffected by changes in a neighboring chunk.
- **Self-contained descent:** Mutation uses its own descent logic (not shared with `range`'s path_frame) because it needs to carry full internal node entries for reconstruction on the way back up.

## Interface

```ocaml
(** tree.mli — additions *)

(** [put store root key value] returns a new root hash with [key]
    mapped to [value]. If [key] already exists, its value is replaced.
    The old tree remains accessible via its original root hash. *)
val put : Store.t -> Hash.t -> string -> string -> Hash.t

(** [delete store root key] returns a new root hash with [key] removed.
    @raise Not_found if [key] does not exist in the tree. *)
val delete : Store.t -> Hash.t -> string -> Hash.t
```

## Algorithm

Both `put` and `delete` follow the same structure: descend, modify, re-chunk, propagate up.

```
mutate(store, root, key, operation):

  1. Descend from root to the leaf containing key:
     At each internal node, find the first entry where entry.key >= key.
     Record the path: list of (internal_entries, child_index) frames.
     At the leaf, record all entries.

  2. Apply the operation to the leaf entries:
     - put: insert (key, value) in sorted position, or replace if exists
     - delete: remove the entry with matching key, raise Not_found if absent

  3. Re-chunk the modified leaf entries:
     Create a fresh Chunker at level 0, feed all modified entries.
     This produces 1 or more leaf chunks (the modified entry might
     introduce or remove a boundary). Store each chunk, collect
     (last_key, hash) pairs as the new parent entries.

  4. Propagate up through the path:
     At each ancestor internal node, replace the old child entry
     (at child_index) with the new parent entries from step 3.
     Re-chunk the modified internal entries at the appropriate level.
     This produces the new parent entries for the next level up.

  5. Continue until we reach the root level.
     If the final level produces exactly 1 chunk, return its hash.
     If it produces multiple chunks, continue building internal
     levels (same as Tree.build's termination logic).
```

## Internal node reconstruction

When we re-chunk a modified leaf, we get back a list of `(last_key, hash)` pairs. This list replaces a single entry in the parent internal node. Three cases:

1. **1 new chunk** (most common) — the modified leaf didn't change boundaries. Replace the old entry with the new one at the same position. Internal node has same number of entries.

2. **Multiple new chunks** — the modification introduced a new boundary. Replace the one old entry with N new entries at that position. Internal node grows.

3. **0 new chunks** — only happens if `delete` removes the last entry from a leaf AND the chunk was the only one. The tree is now empty. Return the empty leaf hash.

After replacing entries in the parent, re-chunk the parent's entries at its level. This can cascade. Propagate up until the top.

```
propagate(path, new_entries, level):
  if path is empty:
    if new_entries has 1 element: return its hash
    else: build internal levels until single root (like Tree.build)

  frame = pop from path
  old_entries = frame.internal_entries (as list)
  splice old_entries: remove entry at frame.index,
    insert new_entries at that position

  re-chunk spliced entries at this level:
    chunker = Chunker.create ~target_size ~level
    feed all entries, collect (last_key, hash) pairs

  propagate(remaining_path, new_parent_entries, level + 1)
```

## Edge cases

- **Put into empty tree:** Descend hits empty leaf. Insert the entry, produce one leaf chunk. New root.
- **Delete last entry:** Leaf becomes empty. Return canonical empty tree hash.
- **Put same key, same value:** Produces identical chunks (content-addressed dedup). Returns same root hash.
- **Delete from single-entry tree:** Back to empty tree.
- **Put causes leaf split:** Re-chunking produces 2+ chunks. Parent gains entries. May cascade up.

## Tests

### Unit tests

- **`put` into empty tree:** Put one entry, verify `find` returns it.
- **`put` new key:** Build 10-entry tree, put a new key, verify `find` returns new value and old entries still present.
- **`put` update existing:** Build tree, put with existing key and new value, verify value changed.
- **`put` same value (no-op):** Put with same key and same value, verify root hash unchanged.
- **`delete` existing key:** Build tree, delete a key, verify `find` returns `None`, other entries intact.
- **`delete` missing key raises:** Verify `Not_found` raised.
- **`delete` last entry:** Delete sole entry from single-entry tree, verify empty tree hash.
- **`put`/`delete` in multi-chunk tree:** 200 entries, put and delete at various positions, verify with `find` and `range`.

### Property tests (QCheck)

- **History independence:** Build tree from sorted pairs. Apply N random puts/deletes. Separately build a tree from scratch with the final data set. Verify same root hash.
- **Put/find round-trip:** Random puts into a tree, every put key is findable.
- **Delete/find round-trip:** Build tree, delete random subset, verify deleted keys return `None`, remaining keys return correct values.
