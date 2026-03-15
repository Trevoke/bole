# Typed Db Layer Design (Tuples Step B)

## What we're building

Make the Db layer speak typed tuples natively. Keys and values become `Tuple.t` instead of raw bytes throughout the Db API. The Db module is the boundary between the typed world (user-facing) and the byte world (tree-level), encoding on the way in and decoding on the way out.

This is the foundation for step C (schemas and cell-level merge). Once Db speaks tuples, merge can compare values field-by-field.

## Design decisions

- **Db is the typed boundary.** Db.put encodes Tuple.t → bytes before calling Tree. Db.find decodes bytes → Tuple.t after. Tree, Diff, Merge stay byte-oriented.
- **Diff and Merge unchanged.** They compare bytes for speed. Decoding only happens at the Db boundary for user-facing results.
- **Db gains its own diff_entry type.** Db.diff returns `Db.diff_entry` with `Tuple.t` values, decoded from Diff.entry's strings.
- **Conflicts carry Tuple.t.** `Db.conflict` uses `Tuple.t option` for base/ours/theirs, decoded from Merge.conflict's strings.
- **CLI wraps strings as [Tuple.String s].** Input stays string-based. Output renders Tuple.t back to strings. Schema-aware parsing comes in step C.

## API changes

```ocaml
(* New type *)
type diff_entry =
  | Added of Tuple.t * Tuple.t
  | Removed of Tuple.t * Tuple.t
  | Modified of Tuple.t * Tuple.t * Tuple.t

(* Updated conflict *)
type conflict = {
  table : string;
  key : Tuple.t;
  base : Tuple.t option;
  ours : Tuple.t option;
  theirs : Tuple.t option;
}

(* Updated signatures *)
val put : t -> table:string -> key:Tuple.t -> value:Tuple.t -> t
val delete : t -> table:string -> key:Tuple.t -> t
val find : t -> table:string -> key:Tuple.t -> Tuple.t option
val range : t -> table:string -> (Tuple.t * Tuple.t) Seq.t
val diff : t -> from:Hash.t -> to_:Hash.t -> table:string -> diff_entry Seq.t
```

## What stays the same

- Tree, Diff, Merge, Store, Chunk, Chunker, Hash, Tuple — all unchanged
- Repo — stores table root hashes, not keys/values
- The encoding/decoding happens only in Db

## CLI changes

- put/delete: wrap argv strings as `[Tuple.String s]`
- get: decode Tuple.t result, render single-value tuples as bare strings
- diff: render tuple values in diff entries
- merge: render tuple values in conflict reports
- Other commands (init, commit, log, branch, switch): unchanged

## Tests

All acceptance tests updated to use Tuple.t. Tests 8-9 (typed key ordering) become simpler — no manual Tuple.encode. CLI e2e test unchanged (string I/O).
