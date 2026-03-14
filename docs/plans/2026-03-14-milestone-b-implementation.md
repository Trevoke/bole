# Milestone B: Commit DAG, Branches, and Three-Way Merge Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Make acceptance test 6 (conflicting three-way merge) pass, which requires building the entire Db layer: table operations, commits, branches, diff, and merge.

**Architecture:** Three new modules — `Db_state` (database state serialization), `Commit` (commit object serialization), `Db` (user-facing database). All stored objects go in the existing content-addressed `Store`. The `Db` module wraps `Store`, `Tree`, and `Diff` with a table name → tree root map, branch management, and three-way merge. Each task makes one more acceptance test pass, building from the bottom up.

**Tech Stack:** OCaml, Alcotest, QCheck2, dune.

**Environment:** Run all commands inside `guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && ...'`. Use `ALCOTEST_COLOR=never` for readable test output.

**Design doc:** `docs/plans/2026-03-14-milestone-b-design.md`

**Acceptance tests:** `test/test_acceptance.ml` (currently commented out of test runner)

**North star:** Acceptance test 6 — conflicting three-way merge. When it passes, all other acceptance tests should pass too.

---

### Task 1: Db_state module (database state serialization)

This is a standalone serialization module with no new dependencies. Like `Chunk`, it has `encode`/`decode` for a simple binary format.

**Files:**
- Create: `lib/db_state.mli`
- Create: `lib/db_state.ml`
- Create: `test/test_db_state.ml`
- Modify: `lib/bole.ml` (add `module Db_state = Db_state`)
- Modify: `test/test_main.ml` (add `Test_db_state.tests`)

**Step 1: Write the test file**

Create `test/test_db_state.ml`:

```ocaml
let test_round_trip () =
  let entries = [
    { Bole.Db_state.name = "posts"; root = Bole.Hash.hash "posts-root" };
    { Bole.Db_state.name = "users"; root = Bole.Hash.hash "users-root" };
  ] in
  let encoded = Bole.Db_state.encode entries in
  let decoded = Bole.Db_state.decode encoded in
  Alcotest.(check int) "same length" 2 (List.length decoded);
  Alcotest.(check string) "first name" "posts" (List.nth decoded 0).name;
  Alcotest.(check string) "second name" "users" (List.nth decoded 1).name;
  Alcotest.(check bool) "first root matches"
    true (Bole.Hash.equal (List.nth decoded 0).root (Bole.Hash.hash "posts-root"));
  Alcotest.(check bool) "second root matches"
    true (Bole.Hash.equal (List.nth decoded 1).root (Bole.Hash.hash "users-root"))

let test_empty_round_trip () =
  let encoded = Bole.Db_state.encode [] in
  let decoded = Bole.Db_state.decode encoded in
  Alcotest.(check int) "empty" 0 (List.length decoded)

let test_sorted_deterministic () =
  let entries_a = [
    { Bole.Db_state.name = "users"; root = Bole.Hash.hash "u" };
    { Bole.Db_state.name = "posts"; root = Bole.Hash.hash "p" };
  ] in
  let entries_b = [
    { Bole.Db_state.name = "posts"; root = Bole.Hash.hash "p" };
    { Bole.Db_state.name = "users"; root = Bole.Hash.hash "u" };
  ] in
  let a = Bole.Db_state.encode entries_a in
  let b = Bole.Db_state.encode entries_b in
  Alcotest.(check string) "sorted deterministic" a b

let tests =
  [ "db_state", [
      Alcotest.test_case "round-trip" `Quick test_round_trip;
      Alcotest.test_case "empty round-trip" `Quick test_empty_round_trip;
      Alcotest.test_case "sorted deterministic" `Quick test_sorted_deterministic;
    ]
  ]
```

**Step 2: Create the interface**

Create `lib/db_state.mli`:

```ocaml
(** Database state: a map of table names to their prolly tree root hashes.

    Serialized as a content-addressed blob in the store. Entries are
    sorted by name for deterministic hashing.

    Binary format: [entry_count: 2B BE] then per entry:
    [name_len: 2B BE] [name] [root_hash: 32B] *)

type table_entry = { name : string; root : Hash.t }

type t = table_entry list

val encode : t -> string
val decode : string -> t
```

**Step 3: Implement**

Create `lib/db_state.ml`:

```ocaml
type table_entry = { name : string; root : Hash.t }

type t = table_entry list

let encode entries =
  let sorted = List.sort (fun a b -> String.compare a.name b.name) entries in
  let buf = Buffer.create 256 in
  let add_u16 n =
    Buffer.add_char buf (Char.chr (n lsr 8 land 0xFF));
    Buffer.add_char buf (Char.chr (n land 0xFF))
  in
  add_u16 (List.length sorted);
  List.iter (fun e ->
    add_u16 (String.length e.name);
    Buffer.add_string buf e.name;
    Buffer.add_string buf (Hash.to_raw_string e.root)
  ) sorted;
  Buffer.contents buf

let decode data =
  let pos = ref 0 in
  let read_u16 () =
    let hi = Char.code data.[!pos] in
    let lo = Char.code data.[!pos + 1] in
    pos := !pos + 2;
    (hi lsl 8) lor lo
  in
  let count = read_u16 () in
  let entries = List.init count (fun _ ->
    let name_len = read_u16 () in
    let name = String.sub data !pos name_len in
    pos := !pos + name_len;
    let root = Hash.of_raw_string (String.sub data !pos Hash.hash_size) in
    pos := !pos + Hash.hash_size;
    { name; root }
  ) in
  entries
```

**Step 4: Wire up**

Add `module Db_state = Db_state` to `lib/bole.ml`. Add `Test_db_state.tests` to `test/test_main.ml`.

**Step 5: Run tests, commit**

Run: `guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && ALCOTEST_COLOR=never dune test'`

```bash
git add lib/db_state.mli lib/db_state.ml lib/bole.ml test/test_db_state.ml test/test_main.ml
git commit -m "feat: add Db_state module for database state serialization"
```

---

### Task 2: Commit module (commit object serialization)

Same pattern as Task 1. Standalone serialization module.

**Files:**
- Create: `lib/commit.mli`
- Create: `lib/commit.ml`
- Create: `test/test_commit.ml`
- Modify: `lib/bole.ml` (add `module Commit = Commit`)
- Modify: `test/test_main.ml` (add `Test_commit.tests`)

**Step 1: Write the test file**

Create `test/test_commit.ml`:

```ocaml
let test_round_trip () =
  let c = {
    Bole.Commit.state = Bole.Hash.hash "state";
    parents = [Bole.Hash.hash "parent1"; Bole.Hash.hash "parent2"];
    message = "test commit";
  } in
  let encoded = Bole.Commit.encode c in
  let decoded = Bole.Commit.decode encoded in
  Alcotest.(check bool) "state matches"
    true (Bole.Hash.equal c.state decoded.Bole.Commit.state);
  Alcotest.(check int) "parent count" 2 (List.length decoded.parents);
  Alcotest.(check bool) "parent1 matches"
    true (Bole.Hash.equal (List.nth c.parents 0) (List.nth decoded.parents 0));
  Alcotest.(check bool) "parent2 matches"
    true (Bole.Hash.equal (List.nth c.parents 1) (List.nth decoded.parents 1));
  Alcotest.(check string) "message matches" "test commit" decoded.message

let test_no_parents () =
  let c = {
    Bole.Commit.state = Bole.Hash.hash "state";
    parents = [];
    message = "initial";
  } in
  let decoded = Bole.Commit.decode (Bole.Commit.encode c) in
  Alcotest.(check int) "no parents" 0 (List.length decoded.Bole.Commit.parents)

let test_empty_message () =
  let c = {
    Bole.Commit.state = Bole.Hash.hash "state";
    parents = [];
    message = "";
  } in
  let decoded = Bole.Commit.decode (Bole.Commit.encode c) in
  Alcotest.(check string) "empty message" "" decoded.Bole.Commit.message

let tests =
  [ "commit", [
      Alcotest.test_case "round-trip" `Quick test_round_trip;
      Alcotest.test_case "no parents" `Quick test_no_parents;
      Alcotest.test_case "empty message" `Quick test_empty_message;
    ]
  ]
```

**Step 2: Create the interface**

Create `lib/commit.mli`:

```ocaml
(** Commit object: a snapshot of database state with history.

    A commit points to a database state (via hash), records parent
    commits, and carries a message. Stored as a content-addressed
    blob in the store.

    Binary format:
    [state_hash: 32B] [parent_count: 2B BE] [parent_hashes: N*32B]
    [message_len: 2B BE] [message] *)

type t = {
  state : Hash.t;
  parents : Hash.t list;
  message : string;
}

val encode : t -> string
val decode : string -> t
```

**Step 3: Implement**

Create `lib/commit.ml`:

```ocaml
type t = {
  state : Hash.t;
  parents : Hash.t list;
  message : string;
}

let encode c =
  let buf = Buffer.create 256 in
  let add_u16 n =
    Buffer.add_char buf (Char.chr (n lsr 8 land 0xFF));
    Buffer.add_char buf (Char.chr (n land 0xFF))
  in
  Buffer.add_string buf (Hash.to_raw_string c.state);
  add_u16 (List.length c.parents);
  List.iter (fun h -> Buffer.add_string buf (Hash.to_raw_string h)) c.parents;
  add_u16 (String.length c.message);
  Buffer.add_string buf c.message;
  Buffer.contents buf

let decode data =
  let pos = ref 0 in
  let read_u16 () =
    let hi = Char.code data.[!pos] in
    let lo = Char.code data.[!pos + 1] in
    pos := !pos + 2;
    (hi lsl 8) lor lo
  in
  let state = Hash.of_raw_string (String.sub data !pos Hash.hash_size) in
  pos := !pos + Hash.hash_size;
  let parent_count = read_u16 () in
  let parents = List.init parent_count (fun _ ->
    let h = Hash.of_raw_string (String.sub data !pos Hash.hash_size) in
    pos := !pos + Hash.hash_size;
    h
  ) in
  let msg_len = read_u16 () in
  let message = String.sub data !pos msg_len in
  { state; parents; message }
```

**Step 4: Wire up, run tests, commit**

Add `module Commit = Commit` to `lib/bole.ml`. Add `Test_commit.tests` to `test/test_main.ml`.

```bash
git add lib/commit.mli lib/commit.ml lib/bole.ml test/test_commit.ml test/test_main.ml
git commit -m "feat: add Commit module for commit object serialization"
```

---

### Task 3: Db module — table operations

Create the `Db` module with `create`, `put`, `delete`, `find`, and the full type definitions (`t`, `conflict`, `merge_result`). This makes **acceptance test 1** (basic table ops) and **test 7** (multi-table) pass.

**Files:**
- Create: `lib/db.mli`
- Create: `lib/db.ml`
- Modify: `lib/bole.ml` (add `module Db = Db`)
- Modify: `test/test_main.ml` (uncomment `Test_acceptance.tests`, or enable tests 1 and 7)

**Step 1: Create the interface**

Create `lib/db.mli` with the full public API (all functions). Functions not yet implemented will raise `failwith "not implemented"`.

```ocaml
(** Diffable, mergeable database backed by prolly trees.

    Each database holds named tables (prolly trees keyed by primary key),
    supports commits, branches, diffs, and three-way merge. *)

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
val store : t -> Store.t
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
```

**Step 2: Implement table operations (stub the rest)**

Create `lib/db.ml`:

```ocaml
module StringMap = Map.Make(String)

type t = {
  store : Store.t;
  branches : (string, Hash.t) Hashtbl.t;
  current_branch : string;
  tables : Hash.t StringMap.t;
}

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

let empty_tree_root store =
  Tree.build store Seq.empty

let create () = {
  store = Store.create ();
  branches = Hashtbl.create 16;
  current_branch = "main";
  tables = StringMap.empty;
}

let store db = db.store

let put db ~table ~key ~value =
  let root = match StringMap.find_opt table db.tables with
    | Some r -> r
    | None -> empty_tree_root db.store
  in
  let root' = Tree.put db.store root key value in
  { db with tables = StringMap.add table root' db.tables }

let delete db ~table ~key =
  let root = match StringMap.find_opt table db.tables with
    | Some r -> r
    | None -> raise Not_found
  in
  let root' = Tree.delete db.store root key in
  { db with tables = StringMap.add table root' db.tables }

let find db ~table ~key =
  match StringMap.find_opt table db.tables with
  | None -> None
  | Some root -> Tree.find db.store root key

let commit _db ~message:_ = failwith "not implemented"
let checkout _db _h = failwith "not implemented"
let parents _db _h = failwith "not implemented"
let branch _db ~name:_ = failwith "not implemented"
let switch _db ~name:_ = failwith "not implemented"
let diff _db ~from:_ ~to_:_ ~table:_ = failwith "not implemented"
let merge _db ~ours:_ ~theirs:_ = failwith "not implemented"
```

**Step 3: Wire up and enable acceptance tests 1 and 7**

Add `module Db = Db` to `lib/bole.ml`. Uncomment `Test_acceptance.tests` in `test/test_main.ml`. Run tests — acceptance tests 1 and 7 should pass, others should fail (but won't crash the runner since Alcotest catches exceptions).

**Step 4: Run tests, commit**

```bash
git add lib/db.mli lib/db.ml lib/bole.ml test/test_main.ml
git commit -m "feat: add Db module with table operations (put/delete/find)"
```

---

### Task 4: Db module — commit, checkout, parents

Implement `commit`, `checkout`, and `parents` using the `Db_state` and `Commit` serialization modules. This makes **acceptance test 2** (commit and history) pass.

**Files:**
- Modify: `lib/db.ml`

**Implementation:**

Replace the three stubs with:

- **`commit`**: serialize `tables` via `Db_state.encode`, store it, get state_hash. Look up current parent from `branches[current_branch]` (if exists). Create `Commit` with state_hash + parents + message, encode and store it, get commit_hash. Update `branches[current_branch] = commit_hash`. Return `(commit_hash, db)`.

- **`checkout`**: load commit via hash, decode. Load db_state via `commit.state`, decode. Build a `StringMap` from the table entries. Return new `t` with those tables. The `current_branch` and `branches` stay the same (checkout is read-only).

- **`parents`**: load commit via hash, decode. Return `commit.parents`.

**Run tests, commit:**

```bash
git add lib/db.ml
git commit -m "feat: add Db.commit, checkout, and parents"
```

---

### Task 5: Db module — branch and switch

Implement `branch` and `switch`. This makes **acceptance test 3** (branching) pass.

**Files:**
- Modify: `lib/db.ml`

**Implementation:**

- **`branch`**: set `branches[name] = branches[current_branch]` (copy current head). Return `{ db with current_branch = name }`.

- **`switch`**: look up `branches[name]` (raise `Not_found` if absent). Load that commit, load its db_state, build tables map. Return new `t` with those tables and `current_branch = name`.

**Run tests, commit:**

```bash
git add lib/db.ml
git commit -m "feat: add Db.branch and switch"
```

---

### Task 6: Db module — diff

Implement `diff`. This makes **acceptance test 4** (diff between commits) pass.

**Files:**
- Modify: `lib/db.ml`

**Implementation:**

- **`diff`**: load both commits, load both db_states. Look up the table root in each (use empty tree root if table absent in a state). Delegate to `Diff.diff store ~from:root_a ~to_:root_b`.

**Run tests, commit:**

```bash
git add lib/db.ml
git commit -m "feat: add Db.diff for per-table commit diffing"
```

---

### Task 7: Db module — three-way merge

This is the big one. Implement `merge` with common ancestor finding, per-table reconciliation, and diff stream merge-join. This makes **acceptance tests 5 and 6** (clean merge and conflicting merge) pass.

**This task will likely need its own brainstorm → design → plan cycle.** The design doc (`docs/plans/2026-03-14-milestone-b-design.md`) describes the algorithm in detail. Key sub-components:

1. **Common ancestor finding** — BFS from both branch heads, walking parent pointers
2. **Per-table reconciliation** — classify each table as unchanged/ours-only/theirs-only/needs-merge
3. **Diff stream merge-join** — walk two `Diff.entry Seq.t` in parallel, detect conflicts

When we reach this task, brainstorm the sub-components and decide whether to break it into smaller tasks or implement as one unit.

**Files:**
- Modify: `lib/db.ml`

**Run tests, commit:**

```bash
git add lib/db.ml
git commit -m "feat: add Db.merge with three-way reconciliation"
```

---

### Task 8: Enable all acceptance tests and verify

Uncomment all acceptance tests. Verify all 7 pass. Clean up any issues.

**Files:**
- Modify: `test/test_main.ml`

```bash
git add test/test_main.ml
git commit -m "test: enable all acceptance tests — milestone B complete"
```
