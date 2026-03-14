# Bole

A version-controlled database. Branch your data, diff any two versions, merge with three-way conflict detection — like git, but for structured key-value data instead of files.

Built on prolly trees (probabilistic B-trees with content-defined chunk boundaries), which give you O(log n) queries, O(d log n) diffs, and structural sharing across versions — all from a single data structure.

For anyone building applications that need to track data history, synchronize datasets, or support branching workflows without the complexity of a full SQL database.

## Try it in 60 seconds

```bash
# Build from source (requires OCaml 5.2+ and opam)
git clone https://github.com/Trevoke/bole && cd bole
opam install . --deps-only
dune build

# Create a database and add some data
bole init
bole put users alice '{"role": "admin", "email": "alice@example.com"}'
bole put users bob '{"role": "editor", "email": "bob@example.com"}'
bole commit -m "Add initial users"

# Check what you have
bole get users alice
bole log
```

You now have a versioned database with one commit. Every value you've stored is content-addressed and immutable — the commit is a snapshot you can always return to.

## Branching and merging

This is where bole gets interesting. Create a branch, make changes independently, and merge them back — with automatic conflict detection.

```bash
# Start a feature branch
bole branch staging
bole put users carol '{"role": "viewer"}'
bole put users alice '{"role": "superadmin"}'
bole commit -m "Add carol, promote alice"

# Switch back to main — carol doesn't exist here
bole switch main
bole get users carol     # not found
bole get users alice     # still admin

# Merge staging into main
bole merge staging
bole get users carol     # now exists
bole get users alice     # now superadmin
```

Both branches share the vast majority of their storage. Only the changed chunks and the path to the root are new — everything else is structurally shared.

## Diffing versions

Compare any two commits to see exactly what changed:

```bash
# See what the last commit changed
bole diff <commit1> <commit2> users
# + carol {"role": "viewer"}
# ~ alice {"role": "admin"} -> {"role": "superadmin"}
```

The diff is efficient — it only reads the chunks that actually differ, not the entire dataset. Two trees with a million rows that differ by 5 rows produce a diff by reading roughly 15 chunks, not a million.

## Conflict detection

When two branches modify the same key differently, bole detects the conflict and tells you:

```bash
# Branch A changes alice's role
bole branch branch-a
bole put users alice '{"role": "editor"}'
bole commit -m "demote alice"

# Branch B (from main) changes alice's role differently
bole switch main
bole branch branch-b
bole put users alice '{"role": "owner"}'
bole commit -m "promote alice"

# Merge — bole detects the conflict
bole merge branch-a
# Merge completed with 1 conflict(s):
#   CONFLICT: users/alice
```

The merge applies all non-conflicting changes automatically. Conflicting keys are reported with the base, ours, and theirs values so you can resolve them.

## How it works

Bole stores each table as a **prolly tree** — a B-tree where chunk boundaries are determined by the content itself (specifically, by hashing the keys). This gives three properties simultaneously:

1. **Fast queries**: O(log n) point lookups and range scans, just like a B-tree.
2. **Fast diffs**: O(d log n) where d is the number of differences. Compare root hashes — if equal, skip the entire subtree. Only descend into subtrees that actually differ.
3. **History independence**: The same set of key-value pairs always produces the same tree structure, regardless of insertion order. This is what makes branching and merging work correctly.

Everything is stored in a content-addressed chunk store (hash → bytes). Commits point to database states, which point to table roots. Branches are just named pointers to commits. Structural sharing means branching is O(1) and storage grows with the size of changes, not the size of the data.

The architecture follows the [Noms](https://github.com/attic-labs/noms)/[Dolt](https://github.com/dolthub/dolt) lineage: prolly trees for table storage, a commit DAG for history, and three-way merge over parallel diff streams.

## Building from source

Bole requires OCaml 5.2+ and uses opam for dependency management.

```bash
git clone https://github.com/Trevoke/bole
cd bole
opam install . --deps-only
dune build
dune test     # 104 tests
```

The binary is at `_build/default/bin/main.exe`, or use `dune exec bin/main.exe -- <command>` to run directly.

### With GNU Guix

If you use Guix for environment management:

```bash
guix shell -m manifest.scm
unset OCAMLPATH    # avoid conflicts between Guix and opam
eval $(opam env)
dune build
```

## Command reference

| Command | Description |
|---------|-------------|
| `bole init` | Create a new repository (`.bole/` directory) |
| `bole put <table> <key> <value>` | Insert or update a key-value pair |
| `bole get <table> <key>` | Look up a value |
| `bole delete <table> <key>` | Remove a key |
| `bole commit -m <message>` | Snapshot current state, prints commit hash |
| `bole log` | Show commit history on current branch |
| `bole branch <name>` | Create and switch to a new branch |
| `bole switch <name>` | Switch to an existing branch |
| `bole diff <commit1> <commit2> <table>` | Show differences between two commits |
| `bole merge <branch>` | Three-way merge a branch into current |

## Library usage

Bole is also usable as an OCaml library:

```ocaml
let db = Bole.Db.create () in
let db = Bole.Db.put db ~table:"users" ~key:"alice" ~value:"admin" in
let hash, db = Bole.Db.commit db ~message:"first" in

let db = Bole.Db.branch db ~name:"feature" in
let db = Bole.Db.put db ~table:"users" ~key:"bob" ~value:"new" in
let _, db = Bole.Db.commit db ~message:"add bob" in

let db = Bole.Db.switch db ~name:"main" in
let result = Bole.Db.merge db ~ours:"main" ~theirs:"feature" in
(* result.db has the merged state, result.conflicts lists any conflicts *)
```

## Known limitations

**In-memory by default.** The library's `Db.create ()` uses an in-memory store. Only the CLI persists to disk (via `.bole/` directory). Programmatic file persistence requires using `Store.create ~path` directly.

**No typed keys or values.** Keys and values are raw bytes (`string`). Order-preserving typed tuple encoding (int64, float, string) is planned but not yet implemented.

**Chunker mean undershoot.** The chunker uses a quadratic hazard ramp for boundary detection. Due to cumulative probability, the actual mean chunk size is 0.5x-0.8x of the `target_size` parameter. A future fix will use a Weibull CDF (shape K=4), matching Dolt's production algorithm.

**No garbage collection.** Content-addressed objects are never deleted. Old, unreferenced chunks accumulate. A GC pass (mark reachable from branch heads, sweep unreachable) is needed for long-lived repositories.

**Single-process only.** No file locking or concurrent access handling. The CLI assumes one process accesses the `.bole/` directory at a time.

## License

[TBD]
