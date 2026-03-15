# Schema-Aware Tables Implementation Plan (Tuples Step C)

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add schema-aware tables with `create_table`, `put_row`, `get_row`, `delete_row`, `range_rows`, and cell-level merge that resolves conflicts when different columns changed on different branches.

**Architecture:** New `Schema` module for table definitions. `Db_state` gains a `schema` hash per table. `Db` replaces raw `put`/`find`/`delete`/`range` with schema-aware row operations. Cell-level merge happens in `Db.merge` when a `Merge.conflict` is received — Db loads the schema, decodes values, compares field-by-field, and resolves non-overlapping column changes.

**Tech Stack:** OCaml, Alcotest, cmdliner, dune.

**Environment:** Run all commands inside `guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && ...'`. Use `ALCOTEST_COLOR=never` for readable test output.

**Design doc:** `docs/plans/2026-03-14-schema-design.md`

---

### Task 1: Schema module

Create the Schema module with type definitions, validation, and binary encode/decode.

**Files:**
- Create: `lib/schema.mli`
- Create: `lib/schema.ml`
- Create: `test/test_schema.ml`
- Modify: `lib/bole.ml` (add `module Schema = Schema`)
- Modify: `test/test_main.ml` (add `Test_schema.tests`)

**Step 1: Create the interface**

Create `lib/schema.mli`:

```ocaml
(** Table schema: column definitions and primary key.

    Binary format:
    [column_count: 2B BE] then per column: [name_len: 2B BE] [name] [type: 1B]
    [pk_count: 2B BE] then per pk column: [name_len: 2B BE] [name]
    Type bytes: 0x01 = Int64, 0x02 = Str *)

type column_type = Int64 | Str

type t = {
  columns : (string * column_type) list;
  primary_key : string list;
}

val create : columns:(string * column_type) list -> primary_key:string list -> t
(** [create ~columns ~primary_key] creates a schema.
    @raise Invalid_argument if any primary key column is not in the column list. *)

val encode : t -> string
val decode : string -> t

val key_columns : t -> (string * column_type) list
(** Returns the primary key columns in declaration order. *)

val value_columns : t -> (string * column_type) list
(** Returns the non-primary-key columns in declaration order. *)
```

**Step 2: Implement**

Create `lib/schema.ml`:

```ocaml
type column_type = Int64 | Str

type t = {
  columns : (string * column_type) list;
  primary_key : string list;
}

let create ~columns ~primary_key =
  List.iter (fun pk ->
    if not (List.mem_assoc pk columns) then
      invalid_arg (Printf.sprintf "Schema.create: primary key column %S not in columns" pk)
  ) primary_key;
  { columns; primary_key }

let key_columns schema =
  List.filter (fun (name, _) -> List.mem name schema.primary_key) schema.columns

let value_columns schema =
  List.filter (fun (name, _) -> not (List.mem name schema.primary_key)) schema.columns

let type_to_byte = function
  | Int64 -> '\x01'
  | Str -> '\x02'

let byte_to_type = function
  | '\x01' -> Int64
  | '\x02' -> Str
  | c -> invalid_arg (Printf.sprintf "Schema.decode: unknown type byte 0x%02x" (Char.code c))

let encode schema =
  let buf = Buffer.create 128 in
  let add_u16 n =
    Buffer.add_char buf (Char.chr (n lsr 8 land 0xFF));
    Buffer.add_char buf (Char.chr (n land 0xFF))
  in
  add_u16 (List.length schema.columns);
  List.iter (fun (name, typ) ->
    add_u16 (String.length name);
    Buffer.add_string buf name;
    Buffer.add_char buf (type_to_byte typ)
  ) schema.columns;
  add_u16 (List.length schema.primary_key);
  List.iter (fun name ->
    add_u16 (String.length name);
    Buffer.add_string buf name
  ) schema.primary_key;
  Buffer.contents buf

let decode data =
  let pos = ref 0 in
  let read_u16 () =
    let hi = Char.code data.[!pos] in
    let lo = Char.code data.[!pos + 1] in
    pos := !pos + 2;
    (hi lsl 8) lor lo
  in
  let col_count = read_u16 () in
  let columns = List.init col_count (fun _ ->
    let name_len = read_u16 () in
    let name = String.sub data !pos name_len in
    pos := !pos + name_len;
    let typ = byte_to_type data.[!pos] in
    pos := !pos + 1;
    (name, typ)
  ) in
  let pk_count = read_u16 () in
  let primary_key = List.init pk_count (fun _ ->
    let name_len = read_u16 () in
    let name = String.sub data !pos name_len in
    pos := !pos + name_len;
    name
  ) in
  { columns; primary_key }
```

**Step 3: Write tests**

Create `test/test_schema.ml`:

```ocaml
let test_round_trip () =
  let s = Bole.Schema.create
    ~columns:["id", Bole.Schema.Int64; "name", Bole.Schema.Str; "email", Bole.Schema.Str]
    ~primary_key:["id"] in
  let decoded = Bole.Schema.decode (Bole.Schema.encode s) in
  Alcotest.(check int) "3 columns" 3 (List.length decoded.columns);
  Alcotest.(check int) "1 pk" 1 (List.length decoded.primary_key);
  Alcotest.(check string) "pk is id" "id" (List.hd decoded.primary_key);
  Alcotest.(check string) "col 0" "id" (fst (List.nth decoded.columns 0));
  Alcotest.(check string) "col 1" "name" (fst (List.nth decoded.columns 1));
  Alcotest.(check string) "col 2" "email" (fst (List.nth decoded.columns 2))

let test_invalid_pk () =
  match Bole.Schema.create ~columns:["id", Bole.Schema.Int64] ~primary_key:["missing"] with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"

let test_key_value_columns () =
  let s = Bole.Schema.create
    ~columns:["id", Bole.Schema.Int64; "name", Bole.Schema.Str; "email", Bole.Schema.Str]
    ~primary_key:["id"] in
  let kc = Bole.Schema.key_columns s in
  let vc = Bole.Schema.value_columns s in
  Alcotest.(check int) "1 key col" 1 (List.length kc);
  Alcotest.(check int) "2 value cols" 2 (List.length vc);
  Alcotest.(check string) "key col is id" "id" (fst (List.hd kc));
  Alcotest.(check string) "val col 0" "name" (fst (List.nth vc 0));
  Alcotest.(check string) "val col 1" "email" (fst (List.nth vc 1))

let test_composite_pk () =
  let s = Bole.Schema.create
    ~columns:["region", Bole.Schema.Str; "id", Bole.Schema.Int64; "name", Bole.Schema.Str]
    ~primary_key:["region"; "id"] in
  let decoded = Bole.Schema.decode (Bole.Schema.encode s) in
  Alcotest.(check int) "2 pk cols" 2 (List.length decoded.primary_key);
  let kc = Bole.Schema.key_columns s in
  Alcotest.(check int) "2 key cols" 2 (List.length kc)

let tests =
  [ "schema", [
      Alcotest.test_case "round-trip" `Quick test_round_trip;
      Alcotest.test_case "invalid pk raises" `Quick test_invalid_pk;
      Alcotest.test_case "key/value columns" `Quick test_key_value_columns;
      Alcotest.test_case "composite pk" `Quick test_composite_pk;
    ]
  ]
```

**Step 4: Wire up**

Add `module Schema = Schema` to `lib/bole.ml` (after `module Tuple = Tuple`, before `module Db = Db`).

Add `Test_schema.tests` to `test/test_main.ml` (after `Test_tuple.tests`, before `Test_repo.tests`).

**Step 5: Run tests and commit**

Run: `guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && ALCOTEST_COLOR=never dune test'`

Expected: All existing 113 tests pass, plus 4 new schema tests = 117.

```bash
git add lib/schema.mli lib/schema.ml lib/bole.ml test/test_schema.ml test/test_main.ml
git commit -m "feat: add Schema module for table definitions"
```

---

### Task 2: Update Db_state for schema hash

Add a `schema` hash field to `Db_state.table_entry`. This is a breaking change to the binary format.

**Files:**
- Modify: `lib/db_state.mli`
- Modify: `lib/db_state.ml`
- Modify: `test/test_db_state.ml`

**Step 1: Update the type and encoding**

In `lib/db_state.mli`:

```ocaml
type table_entry = { name : string; root : Hash.t; schema : Hash.t }
```

In `lib/db_state.ml`, update `table_entry`, and update `encode`/`decode` to include schema_hash (32B) after root_hash:

```ocaml
type table_entry = { name : string; root : Hash.t; schema : Hash.t }

(* encode: add schema hash after root hash *)
List.iter (fun e ->
  add_u16 (String.length e.name);
  Buffer.add_string buf e.name;
  Buffer.add_string buf (Hash.to_raw_string e.root);
  Buffer.add_string buf (Hash.to_raw_string e.schema)
) sorted;

(* decode: read schema hash after root hash *)
let root = Hash.of_raw_string (String.sub data !pos Hash.hash_size) in
pos := !pos + Hash.hash_size;
let schema = Hash.of_raw_string (String.sub data !pos Hash.hash_size) in
pos := !pos + Hash.hash_size;
{ name; root; schema }
```

**Step 2: Update Db_state tests**

Update `test/test_db_state.ml` — all `table_entry` constructions need a `schema` field:

```ocaml
{ Bole.Db_state.name = "posts"; root = Bole.Hash.hash "posts-root"; schema = Bole.Hash.hash "posts-schema" }
```

Update assertions to check schema hashes too.

**Step 3: Run tests**

Note: Db.ml still constructs `Db_state.table_entry` without the schema field — it will fail to compile. That's expected; we fix Db in Task 3. For now, build only the library modules that don't depend on the old API:

Run: `guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && dune build lib/db_state.cmo' 2>&1`

Or just verify the db_state tests compile and pass by temporarily adjusting. Actually, since Db references Db_state, `dune test` will fail. The simplest approach: make all changes to Db_state, then immediately move to Task 3 which updates Db. Commit both together if needed.

Alternative: commit Db_state changes alone, accept that the build is temporarily broken, and fix in Task 3. This is acceptable for a multi-step feature.

```bash
git add lib/db_state.mli lib/db_state.ml test/test_db_state.ml
git commit -m "feat: add schema hash to Db_state table entries"
```

---

### Task 3: Update Db for schema-aware operations

This is the big task. Replace the Db API: remove raw `put`/`find`/`delete`/`range`, add `create_table`/`put_row`/`get_row`/`delete_row`/`range_rows`. Update internal tables map to carry schema hash. Update `commit`/`checkout`/`switch`/`merge`/`diff`/`of_parts`/`working_tables` for the new `table_state` structure.

**Files:**
- Modify: `lib/db.mli`
- Modify: `lib/db.ml`

**Step 1: Update the interface**

Replace `lib/db.mli` entirely:

```ocaml
(** Diffable, mergeable database backed by prolly trees.

    Each database holds named tables with schemas. Tables are prolly
    trees keyed by primary key, with non-key columns as the value.
    Supports commits, branches, diffs, and three-way cell-level merge. *)

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

val create_table : t -> table:string -> schema:Schema.t -> t
val put_row : t -> table:string -> row:(string * Tuple.value) list -> t
val get_row : t -> table:string -> key:Tuple.t -> (string * Tuple.value) list option
val delete_row : t -> table:string -> key:Tuple.t -> t
val range_rows : t -> table:string -> (string * Tuple.value) list Seq.t

val commit : t -> message:string -> Hash.t * t
val checkout : t -> Hash.t -> t
val parents : t -> Hash.t -> Hash.t list
val branch : t -> name:string -> t
val switch : t -> name:string -> t
val diff : t -> from:Hash.t -> to_:Hash.t -> table:string -> diff_entry Seq.t
val merge : t -> ours:string -> theirs:string -> merge_result

(** Internal accessors for Repo module *)
val working_state : t -> (string * Hash.t * Hash.t) list
(** Returns [(table_name, root_hash, schema_hash)] for each table *)

val of_parts :
  store:Store.t ->
  branches:(string * Hash.t) list ->
  current_branch:string ->
  head_commit:Hash.t option ->
  ?working_state:(string * Hash.t * Hash.t) list ->
  unit ->
  t
```

Note: `working_tables` becomes `working_state` returning `(name, root, schema)` triples.

**Step 2: Update the implementation**

Key changes in `lib/db.ml`:

1. Change internal map type:
```ocaml
type table_state = { root : Hash.t; schema : Hash.t }
type t = {
  store : Store.t;
  branches : (string, Hash.t) Hashtbl.t;
  current_branch : string;
  tables : table_state StringMap.t;
}
```

2. Helper to load a schema from store:
```ocaml
let load_schema store schema_hash =
  Schema.decode (Store.get store schema_hash)
```

3. Helper to split a row into key tuple and value tuple:
```ocaml
let split_row schema row =
  let key_cols = Schema.key_columns schema in
  let val_cols = Schema.value_columns schema in
  let key = List.map (fun (name, _) -> List.assoc name row) key_cols in
  let value = List.map (fun (name, _) -> List.assoc name row) val_cols in
  (key, value)

let merge_row schema key_tuple value_tuple =
  let key_cols = Schema.key_columns schema in
  let val_cols = Schema.value_columns schema in
  let key_pairs = List.combine (List.map fst key_cols) key_tuple in
  let val_pairs = List.combine (List.map fst val_cols) value_tuple in
  key_pairs @ val_pairs
```

4. `create_table`:
```ocaml
let create_table db ~table ~schema =
  let schema_data = Schema.encode schema in
  let schema_hash = Store.put db.store schema_data in
  let root = empty_tree_root db.store in
  { db with tables = StringMap.add table { root; schema = schema_hash } db.tables }
```

5. `put_row`:
```ocaml
let put_row db ~table ~row =
  let ts = StringMap.find table db.tables in
  let schema = load_schema db.store ts.schema in
  let key_tuple, val_tuple = split_row schema row in
  let key_bytes = Tuple.encode key_tuple in
  let val_bytes = Tuple.encode val_tuple in
  let root' = Tree.put db.store ts.root key_bytes val_bytes in
  { db with tables = StringMap.add table { ts with root = root' } db.tables }
```

6. `get_row`:
```ocaml
let get_row db ~table ~key =
  match StringMap.find_opt table db.tables with
  | None -> None
  | Some ts ->
    let key_bytes = Tuple.encode key in
    match Tree.find db.store ts.root key_bytes with
    | None -> None
    | Some v ->
      let schema = load_schema db.store ts.schema in
      let val_tuple = Tuple.decode v in
      Some (merge_row schema key val_tuple)
```

7. `delete_row`:
```ocaml
let delete_row db ~table ~key =
  let ts = StringMap.find table db.tables in
  let key_bytes = Tuple.encode key in
  let root' = Tree.delete db.store ts.root key_bytes in
  { db with tables = StringMap.add table { ts with root = root' } db.tables }
```

8. `range_rows`:
```ocaml
let range_rows db ~table =
  match StringMap.find_opt table db.tables with
  | None -> Seq.empty
  | Some ts ->
    let schema = load_schema db.store ts.schema in
    Tree.range db.store ts.root
    |> Seq.map (fun (k, v) ->
      merge_row schema (Tuple.decode k) (Tuple.decode v))
```

9. Update `commit` to use new `Db_state.table_entry` with schema:
```ocaml
let commit db ~message =
  let entries = StringMap.fold (fun name ts acc ->
    Db_state.{ name; root = ts.root; schema = ts.schema } :: acc
  ) db.tables [] in
  (* ... rest unchanged ... *)
```

10. Update `checkout`, `switch`, `load_tables`, `of_parts` to handle `table_state`:
```ocaml
(* In checkout, load_tables, of_parts — build table_state from Db_state entries *)
List.fold_left (fun acc (e : Db_state.table_entry) ->
  StringMap.add e.name { root = e.root; schema = e.schema } acc
) StringMap.empty entries
```

11. Update `working_state`:
```ocaml
let working_state db =
  StringMap.fold (fun name ts acc -> (name, ts.root, ts.schema) :: acc) db.tables []
```

12. Update `of_parts` signature for `working_state`:
```ocaml
let of_parts ~store ~branches ~current_branch ~head_commit ?(working_state=[]) () =
  (* ... *)
  let tables = match working_state with
    | _ :: _ ->
      List.fold_left (fun acc (name, root, schema) ->
        StringMap.add name { root; schema } acc
      ) StringMap.empty working_state
    | [] -> (* load from commit as before, using table_state *)
```

13. Update `merge` — add cell-level resolution. After getting a `Merge.conflict`, attempt field-by-field resolution:
```ocaml
(* In the merge three-way section, after getting result from Merge.three_way *)
(* For each Merge.conflict, try cell-level resolution *)
let ts = StringMap.find name ours_tables in
let schema = load_schema db.store ts.schema in
let val_cols = Schema.value_columns schema in
let n_fields = List.length val_cols in

let try_cell_merge (c : Merge.conflict) =
  match c.base with
  | None -> (* Both added — can't do cell-level *) `Conflict c
  | Some base_bytes ->
    let base_vals = Tuple.decode base_bytes in
    let ours_vals = match c.ours with Some b -> Tuple.decode b | None -> base_vals in
    let theirs_vals = match c.theirs with Some b -> Tuple.decode b | None -> base_vals in
    let merged = List.init n_fields (fun i ->
      let b = List.nth base_vals i in
      let o = List.nth ours_vals i in
      let t = List.nth theirs_vals i in
      if o = b then `Take_theirs t
      else if t = b then `Take_ours o
      else if o = t then `Agree o
      else `Conflict_field
    ) in
    if List.exists (fun x -> x = `Conflict_field) merged then
      `Conflict c
    else
      let merged_vals = List.map (function
        | `Take_theirs v | `Take_ours v | `Agree v -> v
        | `Conflict_field -> assert false
      ) merged in
      `Resolved (c.key, Tuple.encode merged_vals)
in
```

This is complex. The implementer should integrate it into the existing merge flow:
- For each `Merge.conflict`, call `try_cell_merge`
- `Resolved` entries become additional `Tree.put` operations on the merged tree
- `Conflict` entries become `Db.conflict`s as before

**Step 3: Verify it compiles**

Run: `guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && dune build @install' 2>&1`

Tests and CLI will fail to compile — fixed in tasks 4 and 5.

**Step 4: Commit**

```bash
git add lib/db.mli lib/db.ml
git commit -m "feat: schema-aware table operations and cell-level merge"
```

---

### Task 4: Update Repo, acceptance tests, and other tests

Update all callers for the new Db API and Db_state format.

**Files:**
- Modify: `lib/repo.ml`
- Modify: `test/test_acceptance.ml`
- Modify: `test/test_repo.ml`

**Step 1: Update Repo**

In `lib/repo.ml`, update `save` and `load` for the new `working_state` format (triples instead of pairs):

```ocaml
(* In save — working state now includes schema hash *)
let save path db =
  write_file (head_file path) (Db.current_branch db ^ "\n");
  let heads = heads_dir path in
  mkdir_p heads;
  List.iter (fun (name, hash) ->
    write_file (Filename.concat heads name) (Hash.to_hex hash ^ "\n")
  ) (Db.branch_heads db);
  let entries = List.map (fun (name, root, schema) ->
    Db_state.{ name; root; schema }
  ) (Db.working_state db) in
  write_file (working_file path) (Db_state.encode entries)

(* In load — working state from Db_state entries *)
let working_state =
  let wf = working_file path in
  if Sys.file_exists wf then begin
    let data = read_file wf in
    if String.length data > 0 then
      List.map (fun (e : Db_state.table_entry) -> (e.name, e.root, e.schema))
        (Db_state.decode data)
    else []
  end else []
in
Db.of_parts ~store ~branches ~current_branch ~head_commit ~working_state ()
```

**Step 2: Update acceptance tests**

All 9 existing tests need `create_table` calls and `put_row`/`get_row`/`delete_row`/`range_rows` instead of `put`/`find`/`delete`/`range`.

For tests 1-7 (string key, string value), use a simple schema:
```ocaml
let simple_schema = Bole.Schema.create
  ~columns:["key", Bole.Schema.Str; "value", Bole.Schema.Str]
  ~primary_key:["key"]
```

Then:
```ocaml
let db = Bole.Db.create_table db ~table:"users" ~schema:simple_schema in
let db = Bole.Db.put_row db ~table:"users"
  ~row:["key", String "alice"; "value", String "admin"] in
match Bole.Db.get_row db ~table:"users" ~key:[String "alice"] with
| Some row -> (* List.assoc "value" row = String "admin" *)
```

For tests 8-9 (typed keys), use appropriate schemas:
```ocaml
let scores_schema = Bole.Schema.create
  ~columns:["id", Bole.Schema.Int64; "score", Bole.Schema.Str]
  ~primary_key:["id"]
```

Add two new acceptance tests:

**Test 10: Schema-aware table with multi-column value**
```ocaml
let test_schema_aware_table () =
  let db = Bole.Db.create () in
  let schema = Bole.Schema.create
    ~columns:["id", Bole.Schema.Int64; "name", Bole.Schema.Str; "email", Bole.Schema.Str]
    ~primary_key:["id"] in
  let db = Bole.Db.create_table db ~table:"users" ~schema in
  let db = Bole.Db.put_row db ~table:"users"
    ~row:["id", Int64 1L; "name", String "alice"; "email", String "alice@ex.com"] in
  let db = Bole.Db.put_row db ~table:"users"
    ~row:["id", Int64 2L; "name", String "bob"; "email", String "bob@ex.com"] in
  match Bole.Db.get_row db ~table:"users" ~key:[Int64 1L] with
  | Some row ->
    Alcotest.(check tuple_value) "name" (String "alice") (List.assoc "name" row);
    Alcotest.(check tuple_value) "email" (String "alice@ex.com") (List.assoc "email" row)
  | None -> Alcotest.fail "expected row"
```

**Test 11: Cell-level merge**
```ocaml
let test_cell_level_merge () =
  let db = Bole.Db.create () in
  let schema = Bole.Schema.create
    ~columns:["id", Bole.Schema.Int64; "name", Bole.Schema.Str; "email", Bole.Schema.Str]
    ~primary_key:["id"] in
  let db = Bole.Db.create_table db ~table:"users" ~schema in
  let db = Bole.Db.put_row db ~table:"users"
    ~row:["id", Int64 1L; "name", String "alice"; "email", String "old@ex.com"] in
  let _, db = Bole.Db.commit db ~message:"base" in
  (* Branch A: change name *)
  let db = Bole.Db.branch db ~name:"branch-a" in
  let db = Bole.Db.put_row db ~table:"users"
    ~row:["id", Int64 1L; "name", String "Alice Smith"; "email", String "old@ex.com"] in
  let _, db = Bole.Db.commit db ~message:"fix name" in
  (* Branch B: change email *)
  let db = Bole.Db.switch db ~name:"main" in
  let db = Bole.Db.branch db ~name:"branch-b" in
  let db = Bole.Db.put_row db ~table:"users"
    ~row:["id", Int64 1L; "name", String "alice"; "email", String "new@ex.com"] in
  let _, db = Bole.Db.commit db ~message:"fix email" in
  (* Merge — should resolve cleanly via cell-level merge *)
  let result = Bole.Db.merge db ~ours:"branch-b" ~theirs:"branch-a" in
  Alcotest.(check int) "no conflicts" 0 (List.length result.Bole.Db.conflicts);
  match Bole.Db.get_row result.Bole.Db.db ~table:"users" ~key:[Int64 1L] with
  | Some row ->
    Alcotest.(check tuple_value) "merged name" (String "Alice Smith") (List.assoc "name" row);
    Alcotest.(check tuple_value) "merged email" (String "new@ex.com") (List.assoc "email" row)
  | None -> Alcotest.fail "expected merged row"
```

Need a `tuple_value` testable:
```ocaml
let tuple_value = Alcotest.testable
  (fun fmt v -> match v with
    | String s -> Format.fprintf fmt "String %S" s
    | Int64 n -> Format.fprintf fmt "Int64 %Ld" n)
  (=)
```

**Step 3: Update repo tests**

Update `test/test_repo.ml` to use `create_table`/`put_row`/`get_row`.

**Step 4: Run tests**

Run: `guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && ALCOTEST_COLOR=never dune test'`

Expected: All tests pass (113 existing updated + 4 schema + 2 new acceptance = ~119 tests).

**Step 5: Commit**

```bash
git add lib/repo.ml test/test_acceptance.ml test/test_repo.ml
git commit -m "test: update all tests for schema-aware table API"
```

---

### Task 5: Update CLI

Update the CLI for the new schema-aware API. The CLI needs to create tables with schemas and use `put_row`/`get_row`.

**Files:**
- Modify: `bin/main.ml`

**Key changes:**

The CLI currently wraps all input as `[Tuple.String s]`. With schemas, `put` needs to know the schema to construct a proper row. The simplest approach for now:

- `bole put <table> <key> <value>` — still works, but internally creates/uses a simple 2-column schema (key:Str, value:Str). If the table doesn't exist, auto-create it with this schema.
- This maintains CLI backwards compatibility while supporting schemas.

Actually, this conflicts with "schema required" — we can't auto-create. Let's add a `create-table` CLI command:

```
bole create-table <table> --columns "id:int64 name:string email:string" --primary-key "id"
bole put <table> <col1>=<val1> <col2>=<val2> ...
bole get <table> <key1> [<key2> ...]
```

But this is a significant CLI redesign. For now, keep it simple:

- Add `bole create-table <table> <col1>:<type1> <col2>:<type2> ... --pk <col1> [<col2> ...]`
- Change `bole put <table> <col1>=<val1> <col2>=<val2> ...`
- Change `bole get <table> <key_val1> [<key_val2> ...]` — uses schema to know types
- `bole delete <table> <key_val1> [<key_val2> ...]`

The implementer should update `main.ml` with these new command formats. For value parsing, use simple heuristics: if it parses as int64, use Int64; otherwise use String.

Update the `test/test_cli.sh` e2e test to use the new commands.

**Step 1: Implement**

The implementer should update all CLI commands. Key new command:

```ocaml
let create_table_cmd =
  let run table columns pk =
    let path = find_root_or_die () in
    let db = Bole.Repo.load path in
    let parse_col s = match String.split_on_char ':' s with
      | [name; "int64"] -> (name, Bole.Schema.Int64)
      | [name; "string"] -> (name, Bole.Schema.Str)
      | _ -> Printf.eprintf "invalid column: %s (expected name:type)\n" s; exit 1
    in
    let cols = List.map parse_col columns in
    let schema = Bole.Schema.create ~columns:cols ~primary_key:pk in
    let db = Bole.Db.create_table db ~table ~schema in
    Bole.Repo.save path db
  in
  let table = Arg.(required & pos 0 (some string) None & info [] ~docv:"TABLE") in
  let columns = Arg.(non_empty & pos_right 0 string [] & info [] ~docv:"COL:TYPE") in
  let pk = Arg.(value & opt_all string [] & info ["pk"; "primary-key"] ~docv:"COL") in
  let doc = "Create a table with a schema" in
  let info = Cmd.info "create-table" ~doc in
  Cmd.v info Term.(const run $ table $ columns $ pk)
```

**Step 2: Update e2e test**

Update `test/test_cli.sh`:

```bash
$BOLE init
$BOLE create-table users key:string value:string --pk key
$BOLE put users key=alice value=admin
$BOLE get users alice
# etc.
```

**Step 3: Run tests and commit**

```bash
git add bin/main.ml test/test_cli.sh
git commit -m "feat: update CLI for schema-aware tables"
```
