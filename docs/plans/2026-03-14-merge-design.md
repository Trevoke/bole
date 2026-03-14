# Merge Module Design

## What we're building

A `Merge` module that reconciles two diff streams (ancestor→ours and ancestor→theirs) via merge-join, producing non-conflicting changes to apply plus a list of conflicts. Used by `Db.merge` for per-table three-way merge.

## Design decisions

- **Separate Merge module** — not in Diff or Db. Merge is its own concept: reconciliation of two sorted diff streams.
- **Pure stream reconciliation** — Merge knows nothing about Store/Tree/tables. Takes two `Diff.entry Seq.t`, returns changes + conflicts. Db handles the tree/store layer.
- **Changes relative to ours tree** — only theirs-side non-conflicting changes appear in the changes list. Ours-only changes are already in the ours tree.
- **`string option` for conflict values** — `base`, `ours`, `theirs` are all `string option`. `None` means deleted (or never existed). Correct from the start, avoids type changes later when delete-vs-modify conflicts are tested.
- **Merge.conflict has no `table` field** — Merge doesn't know about tables. Db wraps each `Merge.conflict` by adding the table name to produce `Db.conflict`.
- **find_ancestor as private helper in Db** — BFS on commit DAG, separate function for readability but not exposed in .mli.

## Interface

```ocaml
(* merge.mli *)

type change =
  | Put of string * string
  | Delete of string

type conflict = {
  key : string;
  base : string option;
  ours : string option;
  theirs : string option;
}

type result = {
  changes : change list;
  conflicts : conflict list;
}

val three_way : ours:Diff.entry Seq.t -> theirs:Diff.entry Seq.t -> result
```

## Algorithm

Walk both diff streams with cursors advancing by key:

- **Key only in ours** → no action (already in ours tree)
- **Key only in theirs** → apply: Added→Put, Removed→Delete, Modified→Put
- **Key in both, same change** → no action
- **Key in both, different changes** → conflict

Conflict population:
- Both Modified(k, old, new) → `{ key=k; base=Some old; ours=Some ours_new; theirs=Some theirs_new }`
- Both Added(k, v) different values → `{ key=k; base=None; ours=Some v1; theirs=Some v2 }`
- One Removed, other Modified → `{ key=k; base=Some old; ours=None; theirs=Some new }` (or symmetric)

## How Db.merge uses it

1. `find_ancestor` — BFS from both branch heads, first shared commit hash
2. Load three db_states (ancestor, ours, theirs) into StringMaps
3. Union of table names
4. Per table:
   - Ours root = theirs root → keep, skip
   - Ours root = ancestor root → take theirs
   - Theirs root = ancestor root → keep ours
   - All differ → compute two diffs, call `Merge.three_way`, apply changes via Tree.put/Tree.delete, collect conflicts (add table field)
5. Return `{ db; conflicts }`

## Type changes

`Db.conflict` changes to use `string option`:

```ocaml
type conflict = {
  table : string;
  key : string;
  base : string option;
  ours : string option;
  theirs : string option;
}
```

Acceptance test 6 assertions update accordingly.

## Tests

**Merge unit tests** (test/test_merge.ml):
- No changes on either side
- Changes only on theirs → all become changes
- Changes only on ours → empty changes list
- Same change on both → no change, no conflict
- Different changes same key → conflict
- Both added same key differently → conflict with base=None
- Mixed scenario

**Integration tests**: acceptance tests 5 and 6 cover the full pipeline.
