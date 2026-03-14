# Step 8 Design: Diff (Parallel Descent)

## What we're building

A new `Diff` module that computes the differences between two prolly trees by parallel recursive descent, returning a lazy sequence of diff entries. This is the O(d log n) operation that makes prolly trees worthwhile for versioned data.

## Design decisions

- **Separate module:** `Diff` gets its own `diff.ml`/`diff.mli` rather than living in `Tree`. Keeps `Tree` focused and gives diff room to grow when three-way merge arrives in milestone C.
- **Lazy Seq.t return:** Consistent with `Tree.range`. Each pull may trigger `Store.get` calls. Memory-efficient for large diffs.
- **Three-case variant:** `Added`, `Removed`, `Modified` — the three-way merge layer needs to distinguish modifications from add/remove pairs.
- **Labeled arguments for direction:** `~from` (old) and `~to_` (new) make the direction explicit at call sites.
- **Reuse `Tree.range` for one-sided subtrees:** When an entire subtree exists only on one side, use `Tree.range` to stream its leaves rather than writing a dedicated walker.
- **No new exceptions:** `diff` doesn't raise. Store-level failures propagate as they do everywhere else.

## Interface

```ocaml
(* diff.mli *)

type entry =
  | Added of string * string             (* key, value *)
  | Removed of string * string           (* key, value *)
  | Modified of string * string * string  (* key, old_value, new_value *)

val diff : Store.t -> from:Hash.t -> to_:Hash.t -> entry Seq.t
```

## Algorithm

The diff performs a parallel recursive descent of both trees.

```
diff store ~from ~to_:
  if Hash.equal from to_ then Seq.empty
  else diff_nodes store from to_

diff_nodes store h1 h2:
  load both chunks from store
  match (chunk1, chunk2) with
  | (Leaf entries1, Leaf entries2) →
      merge_leaves entries1 entries2
  | (Internal entries1, Internal entries2) →
      merge_internals store entries1 entries2
  | mixed →
      flatten both to leaf entries via Tree.range,
      merge_leaves on the flattened lists
```

### Leaf merge-join

Walk two sorted entry lists with cursors advancing by key:

- Key only in `from` → emit `Removed(key, value)`
- Key only in `to_` → emit `Added(key, value)`
- Key in both, same value → skip
- Key in both, different value → emit `Modified(key, old_value, new_value)`

### Internal merge-join

Walk two sorted internal entry lists by key:

- Key only in `from` → use `Tree.range` on that child, emit all entries as `Removed`
- Key only in `to_` → use `Tree.range` on that child, emit all entries as `Added`
- Key in both, same child hash → skip (O(d log n) comes from here)
- Key in both, different child hash → recursively `diff_nodes` on the two children

Child sequences are concatenated lazily. The recursive structure naturally maps to recursive `Seq.t` construction without needing the mutable state pattern used in `Tree.range`.

### Mixed chunk types

If one root is a leaf and the other is internal (different tree heights), flatten both sides to leaf entries via `Tree.range` and merge-join at the leaf level. This is a rare edge case not worth special-casing.

## Edge cases

- **Both roots equal:** `Seq.empty`, checked before any `Store.get`.
- **One or both trees empty:** Empty tree is a leaf with zero entries. The merge-join emits all entries from the non-empty side as `Added` or `Removed`.
- **Ordering guarantee:** Entries are emitted in key order. Merge-join preserves sorted order, and recursive descent processes subtrees left-to-right.

## Tests

### Unit tests

- **Identical trees:** `diff` returns empty sequence.
- **Empty vs non-empty:** Diffing empty tree against 10-entry tree produces 10 `Added` entries. Reverse produces 10 `Removed`.
- **Single addition:** Build tree, put a new key, diff yields one `Added`.
- **Single deletion:** Build tree, delete a key, diff yields one `Removed`.
- **Single modification:** Build tree, put existing key with new value, diff yields one `Modified` with old and new values.
- **Multiple changes:** Mix of adds, removes, and modifications. Verify all present in correct key order.
- **Multi-chunk trees:** 200 entries with `target_size:20`, several mutations, verify diff captures all changes and nothing more.
- **Key ordering:** Verify diff entries are emitted in sorted key order.

### Property tests (QCheck)

- **Diff completeness:** Build tree A from random pairs, apply random puts/deletes to get tree B. `diff A B` yields exactly the changes applied.
- **Diff symmetry:** `diff ~from:a ~to_:b` and `diff ~from:b ~to_:a` are mirrors — `Added` becomes `Removed` and vice versa, `Modified` swaps old/new values.
- **Apply-diff round-trip:** Applying all diff entries to tree A (put for Added/Modified, delete for Removed) produces a tree with the same root hash as tree B.
