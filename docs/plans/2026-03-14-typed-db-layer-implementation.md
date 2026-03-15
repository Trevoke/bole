# Typed Db Layer Implementation Plan (Tuples Step B)

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Make the Db layer speak typed tuples natively — keys and values become `Tuple.t` throughout the Db API, with encoding/decoding at the Tree boundary.

**Architecture:** Db encodes `Tuple.t → string` via `Tuple.encode` before calling Tree, decodes `string → Tuple.t` via `Tuple.decode` after. Diff and Merge stay byte-oriented. Db gains its own `diff_entry` type with `Tuple.t` values. CLI wraps string arguments as `[Tuple.String s]` and renders tuple results back to strings.

**Tech Stack:** OCaml, Alcotest, cmdliner, dune.

**Environment:** Run all commands inside `guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && ...'`. Use `ALCOTEST_COLOR=never` for readable test output.

**Design doc:** `docs/plans/2026-03-14-typed-db-layer-design.md`

---

### Task 1: Update Db interface and implementation

Change the Db API from `string` to `Tuple.t` for keys and values. Add `Db.diff_entry` type. Update all internal encode/decode boundaries.

**Files:**
- Modify: `lib/db.mli`
- Modify: `lib/db.ml`

**Step 1: Update the interface**

Replace the types and function signatures in `lib/db.mli`:

```ocaml
(** Diffable, mergeable database backed by prolly trees.

    Each database holds named tables (prolly trees keyed by primary key),
    supports commits, branches, diffs, and three-way merge.

    Keys and values are typed tuples ([Tuple.t]). The Db layer encodes
    tuples to bytes for storage and decodes them on retrieval. *)

type t

type diff_entry =
  | Added of Tuple.t * Tuple.t
  | Removed of Tuple.t * Tuple.t
  | Modified of Tuple.t * Tuple.t * Tuple.t

type conflict = {
  table : string;
  key : Tuple.t;
  base : Tuple.t option;
  ours : Tuple.t option;
  theirs : Tuple.t option;
}

type merge_result = {
  db : t;
  conflicts : conflict list;
}

val create : unit -> t
val store : t -> Store.t
val current_branch : t -> string
val branch_heads : t -> (string * Hash.t) list
val working_tables : t -> (string * Hash.t) list
val of_parts :
  store:Store.t ->
  branches:(string * Hash.t) list ->
  current_branch:string ->
  head_commit:Hash.t option ->
  ?working_tables:(string * Hash.t) list ->
  unit ->
  t
val put : t -> table:string -> key:Tuple.t -> value:Tuple.t -> t
val delete : t -> table:string -> key:Tuple.t -> t
val find : t -> table:string -> key:Tuple.t -> Tuple.t option
val range : t -> table:string -> (Tuple.t * Tuple.t) Seq.t
val commit : t -> message:string -> Hash.t * t
val checkout : t -> Hash.t -> t
val parents : t -> Hash.t -> Hash.t list
val branch : t -> name:string -> t
val switch : t -> name:string -> t
val diff : t -> from:Hash.t -> to_:Hash.t -> table:string -> diff_entry Seq.t
val merge : t -> ours:string -> theirs:string -> merge_result
```

**Step 2: Update the implementation**

In `lib/db.ml`, add the `diff_entry` type and update the `conflict` type:

```ocaml
type diff_entry =
  | Added of Tuple.t * Tuple.t
  | Removed of Tuple.t * Tuple.t
  | Modified of Tuple.t * Tuple.t * Tuple.t

type conflict = {
  table : string;
  key : Tuple.t;
  base : Tuple.t option;
  ours : Tuple.t option;
  theirs : Tuple.t option;
}
```

Update `put` to encode:

```ocaml
let put db ~table ~key ~value =
  let key_bytes = Tuple.encode key in
  let value_bytes = Tuple.encode value in
  let root = match StringMap.find_opt table db.tables with
    | Some r -> r
    | None -> empty_tree_root db.store
  in
  let root' = Tree.put db.store root key_bytes value_bytes in
  { db with tables = StringMap.add table root' db.tables }
```

Update `delete` to encode:

```ocaml
let delete db ~table ~key =
  let key_bytes = Tuple.encode key in
  let root = match StringMap.find_opt table db.tables with
    | Some r -> r
    | None -> raise Not_found
  in
  let root' = Tree.delete db.store root key_bytes in
  { db with tables = StringMap.add table root' db.tables }
```

Update `find` to encode key and decode result:

```ocaml
let find db ~table ~key =
  let key_bytes = Tuple.encode key in
  match StringMap.find_opt table db.tables with
  | None -> None
  | Some root ->
    match Tree.find db.store root key_bytes with
    | None -> None
    | Some v -> Some (Tuple.decode v)
```

Update `range` to decode both key and value:

```ocaml
let range db ~table =
  match StringMap.find_opt table db.tables with
  | None -> Seq.empty
  | Some root ->
    Tree.range db.store root
    |> Seq.map (fun (k, v) -> (Tuple.decode k, Tuple.decode v))
```

Update `diff` to decode Diff.entry into Db.diff_entry:

```ocaml
let diff db ~from ~to_ ~table =
  let from_root = table_root_from_commit db from table in
  let to_root = table_root_from_commit db to_ table in
  Diff.diff db.store ~from:from_root ~to_:to_root
  |> Seq.map (fun entry ->
    match entry with
    | Diff.Added (k, v) -> Added (Tuple.decode k, Tuple.decode v)
    | Diff.Removed (k, v) -> Removed (Tuple.decode k, Tuple.decode v)
    | Diff.Modified (k, o, n) ->
      Modified (Tuple.decode k, Tuple.decode o, Tuple.decode n))
```

Update the conflict mapping in `merge` to decode:

```ocaml
      List.iter (fun (c : Merge.conflict) ->
        all_conflicts := {
          table = name;
          key = Tuple.decode c.key;
          base = Option.map Tuple.decode c.base;
          ours = Option.map Tuple.decode c.ours;
          theirs = Option.map Tuple.decode c.theirs;
        } :: !all_conflicts
      ) result.Merge.conflicts
```

**Step 3: Verify it compiles**

Run: `guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && dune build @install' 2>&1`

This should compile the library. Tests and CLI will fail to compile (they still use the old string API) — that's expected, we fix those in subsequent tasks.

**Step 4: Commit**

```bash
git add lib/db.mli lib/db.ml
git commit -m "feat: make Db layer speak typed tuples (Tuple.t keys and values)"
```

---

### Task 2: Update acceptance tests

Convert all 9 acceptance tests from `string` to `Tuple.t`. This is mechanical: `~key:"alice"` becomes `~key:[String "alice"]`, `Some "admin"` becomes `Some [String "admin"]`, etc.

**Files:**
- Modify: `test/test_acceptance.ml`

**Step 1: Update all tests**

For convenience, add a local open at the top of the file:

```ocaml
open Bole.Tuple
```

Then convert each test. Example patterns:

```ocaml
(* put/find *)
let db = Bole.Db.put db ~table:"users" ~key:[String "alice"] ~value:[String "admin"] in

(* find check — need a tuple testable *)
let tuple_testable = Alcotest.testable
  (fun fmt t -> Format.fprintf fmt "%s"
    (String.concat ", " (List.map (function
      | String s -> Printf.sprintf "String %S" s
      | Int64 n -> Printf.sprintf "Int64 %Ld" n
    ) t)))
  (=)
in
let opt_tuple = Alcotest.option tuple_testable in
Alcotest.(check opt_tuple) "find alice"
  (Some [String "admin"]) (Bole.Db.find db ~table:"users" ~key:[String "alice"]);

(* delete *)
let db = Bole.Db.delete db ~table:"users" ~key:[String "bob"] in

(* diff — now returns Db.diff_entry *)
let diff_testable = Alcotest.testable
  (fun fmt e -> match e with
    | Bole.Db.Added (k, v) ->
      Format.fprintf fmt "Added(%a, %a)" ... k ... v
    | Bole.Db.Removed (k, v) ->
      Format.fprintf fmt "Removed(%a, %a)" ... k ... v
    | Bole.Db.Modified (k, o, n) ->
      Format.fprintf fmt "Modified(%a, %a, %a)" ... k ... o ... n)
  (=)
in

(* conflict — now uses Tuple.t *)
Alcotest.(check opt_tuple) "conflict base" (Some [String "v1"]) c.Bole.Db.base;

(* Tests 8-9 get simpler — no more manual Tuple.encode *)
let put db n v =
  Bole.Db.put db ~table:"scores" ~key:[Int64 n] ~value:[String v]
in
(* range returns (Tuple.t * Tuple.t) Seq.t *)
let all = Bole.Db.range db ~table:"scores" |> List.of_seq in
let values = List.map (fun (_, v) ->
  match v with [String s] -> s | _ -> assert false
) all in
```

The implementer should define reusable `tuple_testable` and `opt_tuple` helpers at the top of the file, and a `diff_entry_testable` for diff assertions.

**Step 2: Run tests**

Run: `guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && ALCOTEST_COLOR=never dune test' 2>&1`

Expected: All 113 tests pass (the acceptance tests now use the new Tuple.t API).

Note: Tests may fail to compile at first. The implementer should fix all type errors until the tests compile and pass. The key issue is that `Alcotest.(check (option string))` must become `Alcotest.(check opt_tuple)` everywhere, and diff assertions must use `Bole.Db.diff_entry` instead of `Bole.Diff.entry`.

**Step 3: Commit**

```bash
git add test/test_acceptance.ml
git commit -m "test: update acceptance tests for typed Tuple.t Db API"
```

---

### Task 3: Update CLI

Convert the CLI to wrap string arguments as `[Tuple.String s]` and render Tuple.t results as strings.

**Files:**
- Modify: `bin/main.ml`

**Step 1: Add a render helper**

Add at the top of `bin/main.ml` (after `open Cmdliner`):

```ocaml
let render_tuple t =
  String.concat " " (List.map (function
    | Bole.Tuple.String s -> s
    | Bole.Tuple.Int64 n -> Int64.to_string n
  ) t)
```

**Step 2: Update commands**

**put:** Wrap key and value:

```ocaml
let db = Bole.Db.put db ~table
  ~key:[Bole.Tuple.String key]
  ~value:[Bole.Tuple.String value] in
```

**get:** Decode result:

```ocaml
match Bole.Db.find db ~table ~key:[Bole.Tuple.String key] with
| Some value -> print_string (render_tuple value); print_newline ()
| None -> ...
```

**delete:** Wrap key:

```ocaml
let db = Bole.Db.delete db ~table ~key:[Bole.Tuple.String key] in
```

**diff:** Use `Bole.Db.diff_entry` instead of `Bole.Diff.entry`:

```ocaml
Bole.Db.diff db ~from:h1 ~to_:h2 ~table
|> Seq.iter (fun entry ->
  match entry with
  | Bole.Db.Added (k, v) ->
    Printf.printf "+ %s %s\n" (render_tuple k) (render_tuple v)
  | Bole.Db.Removed (k, v) ->
    Printf.printf "- %s %s\n" (render_tuple k) (render_tuple v)
  | Bole.Db.Modified (k, old_v, new_v) ->
    Printf.printf "~ %s %s -> %s\n" (render_tuple k)
      (render_tuple old_v) (render_tuple new_v))
```

**merge:** Render conflict tuples:

```ocaml
List.iter (fun (c : Bole.Db.conflict) ->
  Printf.printf "  CONFLICT: %s/%s\n" c.table (render_tuple c.key)
) result.Bole.Db.conflicts
```

**Step 3: Build and run e2e test**

Build: `guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && dune build'`

Run e2e test: `guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && bash test/test_cli.sh'`

The e2e test should pass unchanged — the CLI still takes strings and outputs strings. The Tuple wrapping is internal.

**Step 4: Commit**

```bash
git add bin/main.ml
git commit -m "feat: update CLI for typed Tuple.t Db API"
```
