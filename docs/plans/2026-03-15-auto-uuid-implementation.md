# Auto-Generated UUID Primary Keys Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Every table gets an automatic UUIDv7 primary key. Users never manage primary keys. The Db API uses opaque `Uuid.t` for row identification, with separate insert (`put_row`) and update (`update_row`) operations.

**Architecture:** New `Uuid` module for UUIDv7 generation with opaque type. `Tuple.value` gains `Uuid of Uuid.t` variant. `Schema.create` auto-prepends `_id:Uuid` column and sets `primary_key=["_id"]`. Db API changes from `key:Tuple.t` to `id:Uuid.t`, `put_row` returns `Uuid.t * t`, new `update_row` function.

**Tech Stack:** OCaml, Unix (for gettimeofday), Alcotest, cmdliner, dune.

**Environment:** Run all commands inside `guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && ...'`. Use `ALCOTEST_COLOR=never` for readable test output.

**Design doc:** `docs/plans/2026-03-15-auto-uuid-design.md`

---

### Task 1: Uuid module

Create the Uuid module with UUIDv7 generation and opaque type.

**Files:**
- Create: `lib/uuid.mli`
- Create: `lib/uuid.ml`
- Create: `test/test_uuid.ml`
- Modify: `lib/dune` (add `unix` to libraries)
- Modify: `lib/bole.ml` (add `module Uuid = Uuid`)
- Modify: `test/test_main.ml` (add `Test_uuid.tests`)

**Step 1: Create the interface**

Create `lib/uuid.mli`:

```ocaml
(** UUIDv7: time-ordered universally unique identifiers.

    UUIDv7 uses a 48-bit Unix timestamp (milliseconds) prefix followed
    by random bytes, giving chronological sort order under byte comparison.

    Layout (128 bits / 16 bytes):
    - Bytes 0-5: 48-bit timestamp (ms since epoch), big-endian
    - Byte 6: version (0x7X) + 4 bits random
    - Byte 7: 8 bits random
    - Byte 8: variant (0b10XX_XXXX) + 6 bits random
    - Bytes 9-15: 56 bits random *)

type t

val v7 : unit -> t
val equal : t -> t -> bool
val compare : t -> t -> int
val to_raw_string : t -> string
val of_raw_string : string -> t
val to_hex : t -> string
val of_hex : string -> t
val of_string : string -> t
(** Accepts hex with or without dashes. Raises [Invalid_argument] if invalid. *)
```

**Step 2: Implement**

Create `lib/uuid.ml`:

```ocaml
type t = string (* 16 raw bytes *)

let v7 () =
  let ts = Unix.gettimeofday () in
  let ms = Int64.of_float (ts *. 1000.0) in
  let buf = Bytes.create 16 in
  (* Bytes 0-5: 48-bit timestamp, big-endian *)
  for i = 0 to 5 do
    let shift = (5 - i) * 8 in
    Bytes.set buf i (Char.chr (Int64.to_int (Int64.shift_right_logical ms shift) land 0xFF))
  done;
  (* Bytes 6-15: random, then set version and variant bits *)
  for i = 6 to 15 do
    Bytes.set buf i (Char.chr (Random.bits () land 0xFF))
  done;
  (* Byte 6: version 7 (0111 xxxx) — keep low 4 bits random *)
  let b6 = Char.code (Bytes.get buf 6) in
  Bytes.set buf 6 (Char.chr ((b6 land 0x0F) lor 0x70));
  (* Byte 8: variant 10 (10xx xxxx) — keep low 6 bits random *)
  let b8 = Char.code (Bytes.get buf 8) in
  Bytes.set buf 8 (Char.chr ((b8 land 0x3F) lor 0x80));
  Bytes.to_string buf

let equal = String.equal
let compare = String.compare

let to_raw_string t = t

let of_raw_string s =
  if String.length s <> 16 then
    invalid_arg (Printf.sprintf "Uuid.of_raw_string: expected 16 bytes, got %d" (String.length s));
  s

let hex_of_char c =
  let n = Char.code c in
  let hi = n lsr 4 in
  let lo = n land 0x0F in
  let hex_digit d = Char.chr (if d < 10 then d + 0x30 else d - 10 + 0x61) in
  Printf.sprintf "%c%c" (hex_digit hi) (hex_digit lo)

let to_hex t =
  let buf = Buffer.create 32 in
  String.iter (fun c -> Buffer.add_string buf (hex_of_char c)) t;
  Buffer.contents buf

let hex_val c =
  if c >= '0' && c <= '9' then Char.code c - 0x30
  else if c >= 'a' && c <= 'f' then Char.code c - 0x61 + 10
  else if c >= 'A' && c <= 'F' then Char.code c - 0x41 + 10
  else invalid_arg "Uuid: invalid hex character"

let of_hex hex =
  if String.length hex <> 32 then
    invalid_arg (Printf.sprintf "Uuid.of_hex: expected 32 hex chars, got %d" (String.length hex));
  let raw = Bytes.create 16 in
  for i = 0 to 15 do
    let hi = hex_val hex.[i * 2] in
    let lo = hex_val hex.[i * 2 + 1] in
    Bytes.set raw i (Char.chr ((hi lsl 4) lor lo))
  done;
  Bytes.to_string raw

let of_string s =
  (* Strip dashes *)
  let stripped = String.concat "" (String.split_on_char '-' s) in
  of_hex stripped
```

**Step 3: Write tests**

Create `test/test_uuid.ml`:

```ocaml
let test_v7_length () =
  let u = Bole.Uuid.v7 () in
  Alcotest.(check int) "16 bytes" 16 (String.length (Bole.Uuid.to_raw_string u))

let test_v7_unique () =
  let u1 = Bole.Uuid.v7 () in
  let u2 = Bole.Uuid.v7 () in
  Alcotest.(check bool) "unique" false (Bole.Uuid.equal u1 u2)

let test_v7_sortable () =
  let u1 = Bole.Uuid.v7 () in
  Unix.sleepf 0.002;
  let u2 = Bole.Uuid.v7 () in
  Alcotest.(check bool) "chronological order"
    true (Bole.Uuid.compare u1 u2 < 0)

let test_hex_round_trip () =
  let u = Bole.Uuid.v7 () in
  let hex = Bole.Uuid.to_hex u in
  Alcotest.(check int) "32 hex chars" 32 (String.length hex);
  let u2 = Bole.Uuid.of_hex hex in
  Alcotest.(check bool) "round-trip" true (Bole.Uuid.equal u u2)

let test_of_string_dashes () =
  let u = Bole.Uuid.v7 () in
  let hex = Bole.Uuid.to_hex u in
  (* Insert dashes in standard UUID positions: 8-4-4-4-12 *)
  let dashed = Printf.sprintf "%s-%s-%s-%s-%s"
    (String.sub hex 0 8) (String.sub hex 8 4) (String.sub hex 12 4)
    (String.sub hex 16 4) (String.sub hex 20 12) in
  let u2 = Bole.Uuid.of_string dashed in
  Alcotest.(check bool) "dashed round-trip" true (Bole.Uuid.equal u u2)

let test_of_hex_invalid () =
  (match Bole.Uuid.of_hex "tooshort" with
   | exception Invalid_argument _ -> ()
   | _ -> Alcotest.fail "expected Invalid_argument");
  (match Bole.Uuid.of_hex "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz" with
   | exception Invalid_argument _ -> ()
   | _ -> Alcotest.fail "expected Invalid_argument for invalid hex")

let tests =
  [ "uuid", [
      Alcotest.test_case "v7 length" `Quick test_v7_length;
      Alcotest.test_case "v7 unique" `Quick test_v7_unique;
      Alcotest.test_case "v7 sortable" `Quick test_v7_sortable;
      Alcotest.test_case "hex round-trip" `Quick test_hex_round_trip;
      Alcotest.test_case "of_string dashes" `Quick test_of_string_dashes;
      Alcotest.test_case "of_hex invalid" `Quick test_of_hex_invalid;
    ]
  ]
```

**Step 4: Wire up**

Modify `lib/dune` — add `unix`:

```
(library
 (name bole)
 (public_name bole)
 (libraries digestif unix))
```

Add `module Uuid = Uuid` to `lib/bole.ml` — before `module Tuple = Tuple` (since Tuple will depend on Uuid).

Add `Test_uuid.tests` to `test/test_main.ml` — after `Test_tuple.tests`, before `Test_schema.tests`.

**Step 5: Run tests and commit**

Run: `guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && ALCOTEST_COLOR=never dune test'`

Expected: 119 existing + 6 new = 125.

```bash
git add lib/uuid.mli lib/uuid.ml lib/dune lib/bole.ml test/test_uuid.ml test/test_main.ml
git commit -m "feat: add Uuid module with UUIDv7 generation"
```

---

### Task 2: Add Uuid to Tuple and Schema

Add `Uuid of Uuid.t` variant to `Tuple.value` and `Uuid` column type to Schema. Update Schema.create to auto-prepend `_id:Uuid` and set `primary_key=["_id"]`.

**Files:**
- Modify: `lib/tuple.mli`
- Modify: `lib/tuple.ml`
- Modify: `lib/schema.mli`
- Modify: `lib/schema.ml`
- Modify: `test/test_tuple.ml`
- Modify: `test/test_schema.ml`

**Step 1: Update Tuple**

Add to `lib/tuple.mli`:

```ocaml
type value =
  | Int64 of int64
  | String of string
  | Uuid of Uuid.t
```

In `lib/tuple.ml`, add tag `0x03` for Uuid. Encoding: tag + 16 raw bytes. Decoding: read tag, read 16 bytes, construct `Uuid`.

```ocaml
let tag_uuid = '\x03'

(* In encode, add case: *)
| Uuid u ->
  Buffer.add_char buf tag_uuid;
  Buffer.add_string buf (Uuid.to_raw_string u)

(* In decode, add case: *)
| c when c = tag_uuid ->
  let raw = String.sub data !pos 16 in
  pos := !pos + 16;
  values := Uuid (Uuid.of_raw_string raw) :: !values
```

**Step 2: Update Schema**

In `lib/schema.mli`, add `Uuid` column type and change `create` signature:

```ocaml
type column_type = Int64 | Str | Uuid

val create : columns:(string * column_type) list -> t
(* No ~primary_key argument. Auto-prepends ("_id", Uuid) and sets primary_key=["_id"]. *)
```

In `lib/schema.ml`:

```ocaml
type column_type = Int64 | Str | Uuid

let create ~columns =
  let columns = ("_id", Uuid) :: columns in
  let primary_key = ["_id"] in
  { columns; primary_key }

(* Update type_to_byte/byte_to_type for Uuid: *)
let type_to_byte = function
  | Int64 -> '\x01'
  | Str -> '\x02'
  | Uuid -> '\x03'

let byte_to_type = function
  | '\x01' -> Int64
  | '\x02' -> Str
  | '\x03' -> Uuid
  | c -> invalid_arg (...)
```

**Step 3: Update tuple tests**

Add a UUID round-trip test and UUID ordering test to `test/test_tuple.ml`:

```ocaml
let test_uuid_round_trip () =
  let u = Bole.Uuid.v7 () in
  let values = [Bole.Tuple.Uuid u] in
  let decoded = Bole.Tuple.decode (Bole.Tuple.encode values) in
  match decoded with
  | [Bole.Tuple.Uuid u2] -> Alcotest.(check bool) "round-trip" true (Bole.Uuid.equal u u2)
  | _ -> Alcotest.fail "expected Uuid"

let test_uuid_ordering () =
  let u1 = Bole.Uuid.v7 () in
  Unix.sleepf 0.002;
  let u2 = Bole.Uuid.v7 () in
  let a = Bole.Tuple.encode [Bole.Tuple.Uuid u1] in
  let b = Bole.Tuple.encode [Bole.Tuple.Uuid u2] in
  Alcotest.(check bool) "uuid1 < uuid2" true (String.compare a b < 0)
```

Register them in the tests list.

**Step 4: Update schema tests**

All schema tests need updating since `Schema.create` no longer takes `~primary_key` and auto-prepends `_id`. For example:

```ocaml
let test_round_trip () =
  let s = Bole.Schema.create
    ~columns:["name", Bole.Schema.Str; "email", Bole.Schema.Str] in
  let decoded = Bole.Schema.decode (Bole.Schema.encode s) in
  (* Now has 3 columns: _id, name, email *)
  Alcotest.(check int) "3 columns" 3 (List.length decoded.columns);
  Alcotest.(check int) "1 pk" 1 (List.length decoded.primary_key);
  Alcotest.(check string) "pk is _id" "_id" (List.hd decoded.primary_key);
  Alcotest.(check string) "col 0" "_id" (fst (List.nth decoded.columns 0));
  Alcotest.(check string) "col 1" "name" (fst (List.nth decoded.columns 1));
  Alcotest.(check string) "col 2" "email" (fst (List.nth decoded.columns 2))
```

Update all 4 schema tests similarly. The `test_invalid_pk` test should be removed since the user can no longer specify primary keys. Replace with a test that `_id` is always first column:

```ocaml
let test_auto_id_column () =
  let s = Bole.Schema.create ~columns:["name", Bole.Schema.Str] in
  let kc = Bole.Schema.key_columns s in
  Alcotest.(check int) "1 key col" 1 (List.length kc);
  Alcotest.(check string) "key col is _id" "_id" (fst (List.hd kc))
```

**Step 5: Run tests and commit**

Library tests should compile and pass. Db, acceptance tests, and CLI will fail to compile (they still use the old Schema.create and Db API) — that's expected.

Build library only: `guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && dune build lib/tuple.cmo lib/schema.cmo' 2>&1`

If that works, try full test build. If Db compilation fails, that's OK — commit the Tuple/Schema changes and fix Db in Task 3.

```bash
git add lib/tuple.mli lib/tuple.ml lib/schema.mli lib/schema.ml test/test_tuple.ml test/test_schema.ml
git commit -m "feat: add Uuid variant to Tuple and auto-UUID to Schema"
```

---

### Task 3: Update Db for auto-UUID API

Replace `put_row` (now returns `Uuid.t * t`), add `update_row`, change `get_row`/`delete_row` to take `id:Uuid.t`. Update internal key handling — the key is always `[Uuid uuid]`.

**Files:**
- Modify: `lib/db.mli`
- Modify: `lib/db.ml`

**Step 1: Update the interface**

Key changes to `lib/db.mli`:

```ocaml
val put_row : t -> table:string -> row:(string * Tuple.value) list -> Uuid.t * t
(** Insert a new row. Generates a UUIDv7 primary key and returns it. *)

val update_row : t -> table:string -> id:Uuid.t -> row:(string * Tuple.value) list -> t
(** Update an existing row by UUID. *)

val get_row : t -> table:string -> id:Uuid.t -> (string * Tuple.value) list option
(** Look up a row by UUID. Returns full row including _id. *)

val delete_row : t -> table:string -> id:Uuid.t -> t
(** Delete a row by UUID. *)

val range_rows : t -> table:string -> (string * Tuple.value) list Seq.t
(** Scan all rows. Each row includes _id. *)
```

**Step 2: Update the implementation**

In `lib/db.ml`:

```ocaml
let put_row db ~table ~row =
  let ts = StringMap.find table db.tables in
  let uuid = Uuid.v7 () in
  let schema = load_schema db.store ts.schema in
  let val_cols = Schema.value_columns schema in
  let val_tuple = List.map (fun (name, _) -> List.assoc name row) val_cols in
  let key_bytes = Tuple.encode [Uuid uuid] in
  let val_bytes = Tuple.encode val_tuple in
  let root' = Tree.put db.store ts.root key_bytes val_bytes in
  (uuid, { db with tables = StringMap.add table { ts with root = root' } db.tables })

let update_row db ~table ~id ~row =
  let ts = StringMap.find table db.tables in
  let schema = load_schema db.store ts.schema in
  let val_cols = Schema.value_columns schema in
  let val_tuple = List.map (fun (name, _) -> List.assoc name row) val_cols in
  let key_bytes = Tuple.encode [Uuid id] in
  let val_bytes = Tuple.encode val_tuple in
  let root' = Tree.put db.store ts.root key_bytes val_bytes in
  { db with tables = StringMap.add table { ts with root = root' } db.tables }

let get_row db ~table ~id =
  match StringMap.find_opt table db.tables with
  | None -> None
  | Some ts ->
    let key_bytes = Tuple.encode [Uuid id] in
    match Tree.find db.store ts.root key_bytes with
    | None -> None
    | Some v ->
      let schema = load_schema db.store ts.schema in
      let val_tuple = Tuple.decode v in
      Some (merge_row schema [Uuid id] val_tuple)

let delete_row db ~table ~id =
  let ts = StringMap.find table db.tables in
  let key_bytes = Tuple.encode [Uuid id] in
  let root' = Tree.delete db.store ts.root key_bytes in
  { db with tables = StringMap.add table { ts with root = root' } db.tables }

let range_rows db ~table =
  match StringMap.find_opt table db.tables with
  | None -> Seq.empty
  | Some ts ->
    let schema = load_schema db.store ts.schema in
    Tree.range db.store ts.root
    |> Seq.map (fun (k, v) ->
      merge_row schema (Tuple.decode k) (Tuple.decode v))
```

Also update `split_row` — it's no longer used by `put_row` (which generates the key internally). It may still be used by `update_row`... actually no, `update_row` takes the row without `_id` and the id separately. So `split_row` can be removed or simplified. The `merge_row` helper still works — it combines key column names with key values and value column names with value values.

The `diff` function stays the same — it returns `diff_entry` with raw key/value tuples. The key tuple will be `[Uuid ...]` now.

Update the cell-level merge in `merge` — should work as-is since it compares value tuples field-by-field and the key handling is the same (bytes level).

**Step 3: Verify library compiles**

`guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && dune build @install'`

**Step 4: Commit**

```bash
git add lib/db.mli lib/db.ml
git commit -m "feat: auto-UUID primary keys in Db API"
```

---

### Task 4: Update all tests, Repo, and CLI

Update everything that calls the Db API for the new auto-UUID signatures.

**Files:**
- Modify: `test/test_acceptance.ml`
- Modify: `test/test_repo.ml`
- Modify: `lib/repo.ml`
- Modify: `bin/main.ml`
- Modify: `test/test_cli.sh`

**Step 1: Update acceptance tests**

Every `put_row` now returns `(Uuid.t * t)`. Every `get_row`/`delete_row` takes `~id:Uuid.t`. Every `Schema.create` drops `~primary_key`.

Pattern:
```ocaml
(* Before *)
let schema = Bole.Schema.create ~columns:["key", Str; "value", Str] ~primary_key:["key"] in
let db = Bole.Db.put_row db ~table:"users" ~row:["key", String "alice"; "value", String "admin"] in
match Bole.Db.get_row db ~table:"users" ~key:[String "alice"] with

(* After *)
let schema = Bole.Schema.create ~columns:["name", Str; "value", Str] in
let alice_id, db = Bole.Db.put_row db ~table:"users" ~row:["name", String "alice"; "value", String "admin"] in
match Bole.Db.get_row db ~table:"users" ~id:alice_id with
| Some row -> (* List.assoc "value" row = String "admin" *)
```

For the diff test (test 4), diffs will show UUID keys in the entries. The test should check that the right values appear without comparing exact UUIDs (since they're generated).

For the merge tests (5, 6, 11), both branches need to update the SAME row. This requires using `update_row` after the initial `put_row` on the base commit:

```ocaml
(* Base: insert alice *)
let alice_id, db = Bole.Db.put_row db ~table:"users" ~row:["name", String "alice"; ...] in
let _, db = Bole.Db.commit db ~message:"base" in
(* Branch A: update alice's name *)
let db = Bole.Db.branch db ~name:"branch-a" in
let db = Bole.Db.update_row db ~table:"users" ~id:alice_id ~row:["name", String "Alice Smith"; ...] in
```

For range_rows tests (8, 9), the ordering is now by UUID (chronological), not by user-provided keys. These tests need rethinking — they were testing int64/composite key ordering, but now all keys are UUIDs. The user's int64/string columns are value columns.

**IMPORTANT:** Tests 8 and 9 (int64 key ordering, composite key ordering) no longer make sense as-is — the user can't control key ordering since keys are auto-generated UUIDs. These tests should be removed or repurposed to test UUID chronological ordering of `range_rows`.

New test to replace them:
```ocaml
let test_range_rows_chronological () =
  let db = Bole.Db.create () in
  let schema = Bole.Schema.create ~columns:["name", Str] in
  let db = Bole.Db.create_table db ~table:"t" ~schema in
  let _, db = Bole.Db.put_row db ~table:"t" ~row:["name", String "first"] in
  let _, db = Bole.Db.put_row db ~table:"t" ~row:["name", String "second"] in
  let _, db = Bole.Db.put_row db ~table:"t" ~row:["name", String "third"] in
  let rows = Bole.Db.range_rows db ~table:"t" |> List.of_seq in
  let names = List.map (fun row ->
    match List.assoc "name" row with Bole.Tuple.String s -> s | _ -> assert false
  ) rows in
  Alcotest.(check (list string)) "chronological order"
    ["first"; "second"; "third"] names
```

**Step 2: Update Repo**

Repo shouldn't need changes — it works with `Db.working_state` and `Db.of_parts` which deal with root/schema hashes, not keys/values.

**Step 3: Update CLI**

- Remove `--pk` from `create-table`
- `put` takes `col=val` pairs, prints generated UUID
- `get` takes a UUID argument
- `delete` takes a UUID argument
- Add `update` command: `bole update <table> <uuid> col=val ...`

```ocaml
let put_cmd =
  let run table assignments =
    let path = find_root_or_die () in
    let db = Bole.Repo.load path in
    let row = parse_assignments assignments in
    let uuid, db = Bole.Db.put_row db ~table ~row in
    Bole.Repo.save path db;
    Printf.printf "%s\n" (Bole.Uuid.to_hex uuid)
  in
  ...

let get_cmd =
  let run table id_str =
    let path = find_root_or_die () in
    let db = Bole.Repo.load path in
    let id = Bole.Uuid.of_string id_str in
    match Bole.Db.get_row db ~table ~id with
    | Some row -> print_row row
    | None -> Printf.eprintf "not found\n"; exit 1
  in
  ...

let update_cmd =
  let run table id_str assignments =
    let path = find_root_or_die () in
    let db = Bole.Repo.load path in
    let id = Bole.Uuid.of_string id_str in
    let row = parse_assignments assignments in
    let db = Bole.Db.update_row db ~table ~id ~row in
    Bole.Repo.save path db
  in
  ...
```

Add a `render_row` helper and a `parse_assignments` helper:

```ocaml
let parse_assignments assignments =
  List.map (fun s ->
    match String.index_opt s '=' with
    | Some i ->
      let col = String.sub s 0 i in
      let value = String.sub s (i + 1) (String.length s - i - 1) in
      let v = match Int64.of_string_opt value with
        | Some n -> Bole.Tuple.Int64 n
        | None -> Bole.Tuple.String value
      in
      (col, v)
    | None ->
      Printf.eprintf "invalid assignment: %s (expected col=value)\n" s;
      exit 1
  ) assignments

let render_row row =
  String.concat " " (List.map (fun (col, v) ->
    Printf.sprintf "%s=%s" col (match v with
      | Bole.Tuple.String s -> s
      | Bole.Tuple.Int64 n -> Int64.to_string n
      | Bole.Tuple.Uuid u -> Bole.Uuid.to_hex u)
  ) row)
```

**Step 4: Update e2e test**

```bash
#!/bin/bash
set -euo pipefail

dune build bin/main.exe
BOLE="$(pwd)/_build/default/bin/main.exe"

DIR=$(mktemp -d)
cleanup() { rm -rf "$DIR"; }
trap cleanup EXIT
cd "$DIR"

echo "=== init ==="
$BOLE init

echo "=== create-table ==="
$BOLE create-table users name:string email:string

echo "=== put/get ==="
ID1=$($BOLE put users name=alice email=alice@ex.com)
echo "inserted alice: $ID1"
$BOLE get users "$ID1" | grep -q "name=alice"
$BOLE get users "$ID1" | grep -q "email=alice@ex.com"

ID2=$($BOLE put users name=bob email=bob@ex.com)
echo "inserted bob: $ID2"

echo "=== commit ==="
C1=$($BOLE commit -m "initial")
echo "commit1: $C1"

echo "=== update ==="
$BOLE update users "$ID1" name=Alice email=alice@newdomain.com
C2=$($BOLE commit -m "update alice")
echo "commit2: $C2"

echo "=== get after update ==="
$BOLE get users "$ID1" | grep -q "name=Alice"
$BOLE get users "$ID1" | grep -q "email=alice@newdomain.com"

echo "=== log ==="
$BOLE log

echo "=== diff ==="
$BOLE diff "$C1" "$C2" users

echo "=== branch and merge ==="
$BOLE branch feature
ID3=$($BOLE put users name=carol email=carol@ex.com)
$BOLE commit -m "add carol on feature" > /dev/null
$BOLE switch main
$BOLE merge feature
$BOLE get users "$ID3" | grep -q "name=carol"

echo ""
echo "ALL CLI TESTS PASSED"
```

**Step 5: Run all tests and commit**

```bash
guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && ALCOTEST_COLOR=never dune test'
guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && bash test/test_cli.sh'

git add test/test_acceptance.ml test/test_repo.ml lib/repo.ml bin/main.ml test/test_cli.sh
git commit -m "feat: update tests and CLI for auto-UUID primary keys"
```
