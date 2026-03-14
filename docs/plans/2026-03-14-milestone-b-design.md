# Milestone B Design: Commit DAG, Branches, and Three-Way Merge

## What we're building

The layers needed to turn bole from a prolly tree library into a diffable, mergeable database. Three new modules: `Db_state` (database state serialization), `Commit` (commit object serialization), and `Db` (user-facing database with tables, commits, branches, and merge). Driven by acceptance test 6: conflicting three-way merge.

## Design decisions

- **Database state as flat binary blob:** A small map of table name → root hash. Custom binary format, not a prolly tree. The research doc calls it "a small map."
- **Commit as content-addressed object:** Contains a state hash (pointer to database state), parent hashes, and message. Stored in the same content-addressed store as tree chunks.
- **Level of indirection between commit and tables:** Commit → database state → table roots. Follows the research doc architecture. Database state has its own hash identity.
- **Simple BFS for common ancestor:** Walk parent pointers from both branch tips. First shared hash is the ancestor. No criss-cross merge handling (YAGNI).
- **Merge-join of two diff streams:** Three-way merge walks `Diff(ancestor, ours)` and `Diff(ancestor, theirs)` in key order, same merge-join pattern as the diff module.
- **Merge always returns result + conflicts:** `{ db; conflicts }` where `db` has non-conflicting changes applied. Even when there are conflicts, the partial merge is available. Closer to git's model.
- **Table-level reconciliation:** Tables only on one side are kept/added. Table deleted on one side but modified on the other is handled by treating missing tables as empty trees, so deletions become key-level Removed entries handled by the merge-join.
- **Expose all three modules:** `Db_state`, `Commit`, `Db` all re-exported through `bole.ml` for independent testing.

## New modules

### Db_state (database state serialization)

```ocaml
(* db_state.mli *)
type table_entry = { name : string; root : Hash.t }
type t = table_entry list

val encode : t -> string
val decode : string -> t
```

Binary format: `[entry_count: 2B BE] [entries...]` where each entry is `[name_len: 2B BE] [name] [root_hash: 32B]`. List sorted by name for deterministic hashing.

### Commit (commit object serialization)

```ocaml
(* commit.mli *)
type t = {
  state : Hash.t;
  parents : Hash.t list;
  message : string;
}

val encode : t -> string
val decode : string -> t
```

Binary format: `[state_hash: 32B] [parent_count: 2B BE] [parent_hashes: N*32B] [message_len: 2B BE] [message]`.

### Db (user-facing database)

```ocaml
(* db.mli *)
type t

type conflict = {
  table : string;
  key : string;
  base : string;
  ours : string;
  theirs : string;
}

type merge_result = {
  db : t;
  conflicts : conflict list;
}

val create : unit -> t
val put : t -> table:string -> key:string -> value:string -> t
val delete : t -> table:string -> key:string -> t
val find : t -> table:string -> key:string -> string option
val commit : t -> message:string -> Hash.t * t
val checkout : t -> Hash.t -> t
val parents : t -> Hash.t -> Hash.t list
val branch : t -> name:string -> t
val switch : t -> name:string -> t
val diff : t -> from:Hash.t -> to_:Hash.t -> table:string -> Diff.entry Seq.t
val merge : t -> ours:string -> theirs:string -> merge_result
val store : t -> Store.t
```

## Db.t runtime structure

```ocaml
type t = {
  store : Store.t;                        (* shared, mutable *)
  branches : (string, Hash.t) Hashtbl.t;  (* shared, mutable *)
  current_branch : string;                (* immutable per t *)
  tables : Hash.t StringMap.t;            (* immutable working state *)
}
```

- `store`: shared content-addressed storage for chunks, db_states, and commits
- `branches`: mutable branch name → commit hash map, updated by `commit`
- `current_branch`: which branch this `t` is on
- `tables`: working state — table name → tree root hash, updated by `put`/`delete`

## Operation semantics

- **create()**: empty store, empty branches, `current_branch = "main"`, empty tables
- **put/delete/find**: look up table root in `tables` map (empty tree if absent), delegate to `Tree.put`/`Tree.delete`/`Tree.find`, return new `t` with updated map
- **commit**: serialize `tables` as `Db_state`, store it. Create `Commit` with state hash + parent (from `branches[current_branch]`, or `[]` if first commit) + message, store it. Update `branches[current_branch]`. Return `(commit_hash, updated_t)`
- **checkout**: load commit → load db_state → return `t` with those tables
- **branch**: set `branches[name] = branches[current_branch]`, return `t` with `current_branch = name`
- **switch**: load `branches[name]` commit → load db_state → return `t` with those tables and `current_branch = name`
- **diff**: resolve both commits to db_states, look up table root in each (empty tree if absent), delegate to `Diff.diff`
- **merge**: see below

## Three-way merge algorithm

**Step 1: Find common ancestor.** BFS from both `branches[ours]` and `branches[theirs]`, walking parent pointers. First hash in both ancestor sets is the common ancestor. Raise `Not_found` if none exists.

**Step 2: Load three database states.** Ancestor, ours, theirs — each a `StringMap` of table name → root hash.

**Step 3: Union of all table names** across all three states.

**Step 4: Per-table reconciliation:**

- Only in ancestor (both deleted) → skip
- In ancestor + ours, not theirs (theirs deleted):
  - Ours root = ancestor root → accept deletion
  - Ours root differs → treat theirs as empty tree, three-way merge (deletions vs modifications become key-level conflicts)
- Symmetric for ancestor + theirs, not ours
- Only in ours → keep
- Only in theirs → add
- In ours + theirs, not ancestor (both added):
  - Same root → keep
  - Different → three-way merge with empty ancestor
- In all three:
  - Ours root = theirs root → keep (no change)
  - Ours root = ancestor root → take theirs
  - Theirs root = ancestor root → take ours
  - Otherwise → three-way merge

**Step 5: Three-way merge within a table.** Merge-join `Diff(ancestor, ours)` and `Diff(ancestor, theirs)` by key:

- Key in one diff only → apply
- Key in both, same change → apply
- Key in both, different changes → conflict

**Step 6: Return** `{ db; conflicts }` with non-conflicting changes applied in `db`.

## Edge cases

- **First commit**: `parents = []`. BFS handles reaching DAG root.
- **No common ancestor**: raise `Not_found`.
- **Non-existent branch**: `switch`/`merge` raise `Not_found`.
- **Uncommitted working state**: merge operates on committed branch heads only.
- **Missing table treated as empty tree**: `Tree.build store Seq.empty` gives the empty root. Consistent key-level diff/merge behavior.
