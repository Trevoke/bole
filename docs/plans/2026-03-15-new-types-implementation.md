# New Types Implementation Plan: Bool, Float, Timestamp, Blob + Byte-Stuffing

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add Bool, Float, Timestamp, and Blob types to Tuple and Schema. Fix String encoding to use byte-stuffing for embedded null byte support. Version bump to 0.2.0 with changelog.

**Architecture:** Add four new variants to `Tuple.value` and `Schema.column_type` with order-preserving binary encodings. Replace String's null-terminator encoding with byte-stuffing (shared with Blob). Update CLI rendering/parsing. Create CHANGELOG.md and bump version.

**Tech Stack:** OCaml, Alcotest, dune.

**Environment:** Run all commands inside `guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && ...'`. Use `ALCOTEST_COLOR=never` for readable test output.

**Design doc:** `docs/plans/2026-03-15-new-types-design.md`

---

### Task 1: Fix String encoding + add byte-stuffing helpers

Fix String to use byte-stuffing instead of raw null-terminator. Add shared helpers for byte-stuffing encode/decode that Blob will also use.

**Files:**
- Modify: `lib/tuple.ml`
- Modify: `lib/tuple.mli` (update docstring)
- Modify: `test/test_tuple.ml`

**Step 1: Add byte-stuffing helpers to tuple.ml**

Add these helpers before the `encode` function:

```ocaml
(* Byte-stuffing: \x00 in data → \x00\xFF, terminated by \x00\x00 *)
let encode_byte_stuffed buf s =
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    if c = '\x00' then begin
      Buffer.add_char buf '\x00';
      Buffer.add_char buf '\xFF'
    end else
      Buffer.add_char buf c
  done;
  Buffer.add_char buf '\x00';
  Buffer.add_char buf '\x00'

let decode_byte_stuffed data pos =
  let buf = Buffer.create 64 in
  let continue = ref true in
  while !continue do
    let c = data.[!pos] in
    if c = '\x00' then begin
      let next = data.[!pos + 1] in
      if next = '\x00' then begin
        (* Terminator *)
        pos := !pos + 2;
        continue := false
      end else if next = '\xFF' then begin
        (* Escaped null *)
        Buffer.add_char buf '\x00';
        pos := !pos + 2
      end else
        invalid_arg "Tuple.decode: invalid byte-stuffing sequence"
    end else begin
      Buffer.add_char buf c;
      pos := !pos + 1
    end
  done;
  Buffer.contents buf
```

**Step 2: Update String encoding to use byte-stuffing**

In `encode`, change the String case:

```ocaml
| String s ->
  Buffer.add_char buf tag_string;
  encode_byte_stuffed buf s
```

In `decode`, change the String case:

```ocaml
| c when c = tag_string ->
  let s = decode_byte_stuffed data pos in
  values := String s :: !values
```

Note: `decode_byte_stuffed` takes `pos` as a mutable `ref int` — it advances `pos` past the terminator.

**Step 3: Update the mli docstring**

Change `String (tag 0x02): bytes + 0x00 terminator` to `String (tag 0x02): byte-stuffed + 0x00 0x00 terminator`.

**Step 4: Add tests for null bytes in strings**

Add to `test/test_tuple.ml`:

```ocaml
let test_string_with_null () =
  let s = "hello\x00world" in
  let values = [Bole.Tuple.String s] in
  let decoded = Bole.Tuple.decode (Bole.Tuple.encode values) in
  match decoded with
  | [Bole.Tuple.String s2] -> Alcotest.(check string) "null round-trip" s s2
  | _ -> Alcotest.fail "expected String"

let test_string_all_nulls () =
  let s = "\x00\x00\x00" in
  let values = [Bole.Tuple.String s] in
  let decoded = Bole.Tuple.decode (Bole.Tuple.encode values) in
  match decoded with
  | [Bole.Tuple.String s2] -> Alcotest.(check string) "all nulls round-trip" s s2
  | _ -> Alcotest.fail "expected String"
```

Register them in the tests list.

**Step 5: Run tests and commit**

Run: `guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && ALCOTEST_COLOR=never dune test'`

All existing tests should pass (byte-stuffing is backward-compatible for strings without null bytes — they encode slightly differently but still round-trip). The new null-byte tests should pass too.

```bash
git add lib/tuple.ml lib/tuple.mli test/test_tuple.ml
git commit -m "feat: byte-stuffing for String encoding (handles embedded null bytes)"
```

---

### Task 2: Add Bool, Float, Timestamp, Blob to Tuple

Add the four new variants to `Tuple.value` with their encodings.

**Files:**
- Modify: `lib/tuple.mli`
- Modify: `lib/tuple.ml`
- Modify: `test/test_tuple.ml`

**Step 1: Update the interface**

In `lib/tuple.mli`:

```ocaml
type value =
  | Int64 of int64
  | String of string
  | Uuid of Uuid.t
  | Bool of bool
  | Float of float
  | Timestamp of int64   (** Unix timestamp in microseconds *)
  | Blob of string       (** Arbitrary bytes *)
```

Update the docstring to list all tags: 0x01-0x07.

**Step 2: Add tags and encoding cases**

In `lib/tuple.ml`, add tags:

```ocaml
let tag_bool = '\x04'
let tag_float = '\x05'
let tag_timestamp = '\x06'
let tag_blob = '\x07'
```

Add encoding cases in `encode`:

```ocaml
| Bool b ->
  Buffer.add_char buf tag_bool;
  Buffer.add_char buf (if b then '\x01' else '\x00')
| Float f ->
  if Float.is_nan f then invalid_arg "Tuple.encode: NaN not allowed";
  Buffer.add_char buf tag_float;
  let bits = Int64.bits_of_float f in
  let encoded =
    if Int64.shift_right_logical bits 63 = 1L then
      (* Negative: flip all bits *)
      Int64.lognot bits
    else
      (* Positive: flip sign bit *)
      Int64.logxor bits Int64.min_int
  in
  for i = 7 downto 0 do
    let byte = Int64.to_int (Int64.shift_right_logical encoded (i * 8)) land 0xFF in
    Buffer.add_char buf (Char.chr byte)
  done
| Timestamp ts ->
  Buffer.add_char buf tag_timestamp;
  let flipped = Int64.logxor ts Int64.min_int in
  for i = 7 downto 0 do
    let byte = Int64.to_int (Int64.shift_right_logical flipped (i * 8)) land 0xFF in
    Buffer.add_char buf (Char.chr byte)
  done
| Blob b ->
  Buffer.add_char buf tag_blob;
  encode_byte_stuffed buf b
```

Add decoding cases in `decode`:

```ocaml
| c when c = tag_bool ->
  let b = data.[!pos] <> '\x00' in
  pos := !pos + 1;
  values := Bool b :: !values
| c when c = tag_float ->
  let encoded = ref 0L in
  for i = 0 to 7 do
    let byte = Char.code data.[!pos + i] in
    encoded := Int64.logor (Int64.shift_left !encoded 8) (Int64.of_int byte)
  done;
  pos := !pos + 8;
  let bits =
    if Int64.shift_right_logical !encoded 63 = 1L then
      (* Was positive: flip sign bit *)
      Int64.logxor !encoded Int64.min_int
    else
      (* Was negative: flip all bits *)
      Int64.lognot !encoded
  in
  values := Float (Int64.float_of_bits bits) :: !values
| c when c = tag_timestamp ->
  let flipped = ref 0L in
  for i = 0 to 7 do
    let byte = Char.code data.[!pos + i] in
    flipped := Int64.logor (Int64.shift_left !flipped 8) (Int64.of_int byte)
  done;
  pos := !pos + 8;
  let ts = Int64.logxor !flipped Int64.min_int in
  values := Timestamp ts :: !values
| c when c = tag_blob ->
  let b = decode_byte_stuffed data pos in
  values := Blob b :: !values
```

**Step 3: Write tests**

Add to `test/test_tuple.ml`:

```ocaml
let test_bool_round_trip () =
  let values = [Bole.Tuple.Bool true; Bole.Tuple.Bool false] in
  let decoded = Bole.Tuple.decode (Bole.Tuple.encode values) in
  match decoded with
  | [Bole.Tuple.Bool true; Bole.Tuple.Bool false] -> ()
  | _ -> Alcotest.fail "expected Bool true, Bool false"

let test_bool_ordering () =
  let a = Bole.Tuple.encode [Bole.Tuple.Bool false] in
  let b = Bole.Tuple.encode [Bole.Tuple.Bool true] in
  Alcotest.(check bool) "false < true" true (String.compare a b < 0)

let test_float_round_trip () =
  let values = [Bole.Tuple.Float 3.14; Bole.Tuple.Float (-2.718)] in
  let decoded = Bole.Tuple.decode (Bole.Tuple.encode values) in
  match decoded with
  | [Bole.Tuple.Float a; Bole.Tuple.Float b] ->
    Alcotest.(check (float 0.001)) "pi" 3.14 a;
    Alcotest.(check (float 0.001)) "neg e" (-2.718) b
  | _ -> Alcotest.fail "expected two Floats"

let test_float_ordering () =
  let encode_one f = Bole.Tuple.encode [Bole.Tuple.Float f] in
  let neg_inf = encode_one neg_infinity in
  let neg_one = encode_one (-1.0) in
  let neg_zero = encode_one (-0.0) in
  let pos_zero = encode_one 0.0 in
  let pos_one = encode_one 1.0 in
  let pos_inf = encode_one infinity in
  Alcotest.(check bool) "-inf < -1" true (String.compare neg_inf neg_one < 0);
  Alcotest.(check bool) "-1 < -0" true (String.compare neg_one neg_zero < 0);
  Alcotest.(check bool) "-0 < +0" true (String.compare neg_zero pos_zero < 0);
  Alcotest.(check bool) "+0 < +1" true (String.compare pos_zero pos_one < 0);
  Alcotest.(check bool) "+1 < +inf" true (String.compare pos_one pos_inf < 0)

let test_float_nan_rejected () =
  match Bole.Tuple.encode [Bole.Tuple.Float Float.nan] with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument for NaN"

let test_timestamp_round_trip () =
  let ts = 1710500000000000L in (* some timestamp in microseconds *)
  let values = [Bole.Tuple.Timestamp ts] in
  let decoded = Bole.Tuple.decode (Bole.Tuple.encode values) in
  match decoded with
  | [Bole.Tuple.Timestamp ts2] ->
    Alcotest.(check int64) "timestamp round-trip" ts ts2
  | _ -> Alcotest.fail "expected Timestamp"

let test_timestamp_ordering () =
  let a = Bole.Tuple.encode [Bole.Tuple.Timestamp 1000L] in
  let b = Bole.Tuple.encode [Bole.Tuple.Timestamp 2000L] in
  Alcotest.(check bool) "earlier < later" true (String.compare a b < 0)

let test_blob_round_trip () =
  let b = "\x00\x01\x02\xFF\x00" in
  let values = [Bole.Tuple.Blob b] in
  let decoded = Bole.Tuple.decode (Bole.Tuple.encode values) in
  match decoded with
  | [Bole.Tuple.Blob b2] -> Alcotest.(check string) "blob round-trip" b b2
  | _ -> Alcotest.fail "expected Blob"

let test_blob_ordering () =
  let encode_one b = Bole.Tuple.encode [Bole.Tuple.Blob b] in
  let a = encode_one "" in
  let b = encode_one "ab" in
  let c = encode_one "abc" in
  let d = encode_one "b" in
  Alcotest.(check bool) "empty < ab" true (String.compare a b < 0);
  Alcotest.(check bool) "ab < abc" true (String.compare b c < 0);
  Alcotest.(check bool) "abc < b" true (String.compare c d < 0)
```

Register all new tests in the tests list.

**Step 4: Run tests and commit**

Expected: All existing tests + ~11 new type tests pass.

```bash
git add lib/tuple.mli lib/tuple.ml test/test_tuple.ml
git commit -m "feat: add Bool, Float, Timestamp, Blob types to Tuple"
```

---

### Task 3: Add new types to Schema

Add `Bool | Float | Timestamp | Blob` to `Schema.column_type` with type bytes.

**Files:**
- Modify: `lib/schema.mli`
- Modify: `lib/schema.ml`

**Step 1: Update column_type**

In both `.mli` and `.ml`:

```ocaml
type column_type = Int64 | Str | Uuid | Bool | Float | Timestamp | Blob
```

**Step 2: Update type_to_byte / byte_to_type**

```ocaml
let type_to_byte = function
  | Int64 -> '\x01'
  | Str -> '\x02'
  | Uuid -> '\x03'
  | Bool -> '\x04'
  | Float -> '\x05'
  | Timestamp -> '\x06'
  | Blob -> '\x07'

let byte_to_type = function
  | '\x01' -> Int64
  | '\x02' -> Str
  | '\x03' -> Uuid
  | '\x04' -> Bool
  | '\x05' -> Float
  | '\x06' -> Timestamp
  | '\x07' -> Blob
  | c -> invalid_arg (...)
```

**Step 3: Run tests and commit**

All existing tests should pass (no schema tests use new types, but the code compiles).

```bash
git add lib/schema.mli lib/schema.ml
git commit -m "feat: add Bool, Float, Timestamp, Blob to Schema column types"
```

---

### Task 4: Update CLI for new types

Update `render_tuple`, `render_row`, `parse_assignments`, and `create-table` column parsing for the new types.

**Files:**
- Modify: `bin/main.ml`

**Step 1: Update render functions**

```ocaml
let render_tuple t =
  String.concat " " (List.map (function
    | Bole.Tuple.String s -> s
    | Bole.Tuple.Int64 n -> Int64.to_string n
    | Bole.Tuple.Uuid u -> Bole.Uuid.to_hex u
    | Bole.Tuple.Bool b -> string_of_bool b
    | Bole.Tuple.Float f -> Float.to_string f
    | Bole.Tuple.Timestamp ts ->
      (* Format as ISO 8601 *)
      let secs = Int64.to_float ts /. 1_000_000.0 in
      let tm = Unix.gmtime secs in
      Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
        (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
        tm.tm_hour tm.tm_min tm.tm_sec
    | Bole.Tuple.Blob b ->
      (* Hex-encode blob for display *)
      let buf = Buffer.create (String.length b * 2) in
      String.iter (fun c ->
        Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c))
      ) b;
      Buffer.contents buf
  ) t)
```

Update `render_row` similarly (same match cases in the value rendering part).

**Step 2: Update parse_assignments**

The value parsing currently tries `Int64.of_string_opt` then falls back to String. Add more type detection:

```ocaml
let parse_value s =
  (* Try bool *)
  if s = "true" then Bole.Tuple.Bool true
  else if s = "false" then Bole.Tuple.Bool false
  (* Try int64 *)
  else match Int64.of_string_opt s with
  | Some n -> Bole.Tuple.Int64 n
  (* Try float — must contain '.' to avoid int-as-float *)
  | None ->
    if String.contains s '.' then
      match Float.of_string_opt s with
      | Some f -> Bole.Tuple.Float f
      | None -> Bole.Tuple.String s
    else
      Bole.Tuple.String s
```

Note: Timestamp and Blob are harder to auto-detect from CLI strings. For now, they're entered as:
- Timestamp: use int64 microseconds (auto-detected as Int64, user can cast)
- Blob: not supported from CLI (would need hex encoding, deferred)

**Step 3: Update create-table column type parsing**

In the `create_table_cmd`, add new type names:

```ocaml
let parse_col s =
  match String.split_on_char ':' s with
  | [name; "int64"] -> (name, Bole.Schema.Int64)
  | [name; "string"] -> (name, Bole.Schema.Str)
  | [name; "bool"] -> (name, Bole.Schema.Bool)
  | [name; "float"] -> (name, Bole.Schema.Float)
  | [name; "timestamp"] -> (name, Bole.Schema.Timestamp)
  | [name; "blob"] -> (name, Bole.Schema.Blob)
  | _ ->
    Printf.eprintf "invalid column spec: %s (expected name:type)\n" s;
    exit 1
in
```

**Step 4: Run tests and commit**

```bash
git add bin/main.ml
git commit -m "feat: update CLI for Bool, Float, Timestamp, Blob types"
```

---

### Task 5: Acceptance test for new types

Add an acceptance test that creates a table with all types, inserts a row, and reads it back.

**Files:**
- Modify: `test/test_acceptance.ml`

**Step 1: Add the test**

```ocaml
let test_all_types () =
  let db = Bole.Db.create () in
  let schema = Bole.Schema.create ~columns:[
    "name", Bole.Schema.Str;
    "age", Bole.Schema.Int64;
    "active", Bole.Schema.Bool;
    "score", Bole.Schema.Float;
    "created", Bole.Schema.Timestamp;
    "avatar", Bole.Schema.Blob;
  ] in
  let db = Bole.Db.create_table db ~table:"users" ~schema in
  let avatar_data = "\x89PNG\x00\x01\x02" in
  let created_ts = 1710500000000000L in
  let _id, db = Bole.Db.put_row db ~table:"users" ~row:[
    "name", String "alice";
    "age", Int64 30L;
    "active", Bool true;
    "score", Float 98.5;
    "created", Timestamp created_ts;
    "avatar", Blob avatar_data;
  ] in
  match Bole.Db.get_row db ~table:"users" ~id:_id with
  | Some row ->
    Alcotest.(check tuple_value) "name" (String "alice") (List.assoc "name" row);
    Alcotest.(check tuple_value) "age" (Int64 30L) (List.assoc "age" row);
    Alcotest.(check tuple_value) "active" (Bool true) (List.assoc "active" row);
    (match List.assoc "score" row with
     | Float f -> Alcotest.(check (float 0.001)) "score" 98.5 f
     | _ -> Alcotest.fail "expected Float");
    Alcotest.(check tuple_value) "created" (Timestamp created_ts) (List.assoc "created" row);
    (match List.assoc "avatar" row with
     | Blob b -> Alcotest.(check string) "avatar" avatar_data b
     | _ -> Alcotest.fail "expected Blob")
  | None -> Alcotest.fail "expected row"
```

Note: the `tuple_value` testable needs updating to handle new types. Update it at the top of `test_acceptance.ml`:

```ocaml
let tuple_value = Alcotest.testable
  (fun fmt v -> match v with
    | String s -> Format.fprintf fmt "String %S" s
    | Int64 n -> Format.fprintf fmt "Int64 %Ld" n
    | Uuid u -> Format.fprintf fmt "Uuid %s" (Bole.Uuid.to_hex u)
    | Bool b -> Format.fprintf fmt "Bool %b" b
    | Float f -> Format.fprintf fmt "Float %f" f
    | Timestamp ts -> Format.fprintf fmt "Timestamp %Ld" ts
    | Blob b -> Format.fprintf fmt "Blob(%d bytes)" (String.length b))
  (=)
```

Register the test.

**Step 2: Run tests and commit**

```bash
git add test/test_acceptance.ml
git commit -m "test: add acceptance test for all types (Bool, Float, Timestamp, Blob)"
```

---

### Task 6: Changelog and version bump to 0.2.0

Create CHANGELOG.md, bump version in dune-project and bin/main.ml, tag and push.

**Files:**
- Create: `CHANGELOG.md`
- Modify: `bin/main.ml` (version string)
- Modify: `dune-project` (if version is specified there)

**Step 1: Create CHANGELOG.md**

```markdown
# Changelog

All notable changes to this project will be documented in this file.

## [0.2.0] - 2026-03-15

### Added
- **Schema-aware tables**: `create_table` with column definitions, `put_row`/`get_row`/`update_row`/`delete_row`/`range_rows` with named columns
- **Cell-level merge**: branches modifying different columns of the same row merge cleanly
- **Auto-generated UUIDv7 primary keys**: every table gets an `_id` column automatically
- **Uuid module**: UUIDv7 generation with opaque type, hex/string conversion
- **New types**: Bool, Float (IEEE 754, NaN rejected), Timestamp (microseconds), Blob (arbitrary bytes)
- **Byte-stuffing encoding**: String and Blob handle embedded null bytes correctly
- **Order-preserving encoding**: all types sort correctly under byte comparison
- **CLI `create-table` command**: `bole create-table <table> <col:type>...`
- **CLI `update` command**: `bole update <table> <uuid> col=val...`

### Changed
- **Schema required for all tables**: tables must be created with `create_table` before inserting data
- **Db API uses Tuple.t**: keys and values are typed tuples, not raw strings
- **Db API uses Uuid.t**: `get_row`/`delete_row`/`update_row` take `~id:Uuid.t`
- **String encoding**: switched from null-terminator to byte-stuffing (breaking change to on-disk format)
- **CLI `put`**: now takes `col=val` pairs and prints generated UUID
- **CLI `get`/`delete`**: now take UUID argument

## [0.1.0] - 2026-03-14

### Added
- Prolly tree library: Hash (BLAKE2s-256), Store, Chunk, Chunker, Tree
- Diff: O(d log n) parallel descent
- Merge: three-way diff stream reconciliation
- Database layer: Db with named tables, commits, branches
- File-backed persistence in `.bole/` directory
- CLI with 10 subcommands: init, put, get, delete, commit, log, branch, switch, diff, merge
- Cross-platform binaries (Linux, macOS, Windows)
```

**Step 2: Update version in main.ml**

In `bin/main.ml`, find `~version:"0.1.0"` and change to `~version:"0.2.0"`.

**Step 3: Run all tests and e2e**

```bash
guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && ALCOTEST_COLOR=never dune test'
guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && bash test/test_cli.sh'
```

**Step 4: Commit, tag, push**

```bash
git add CHANGELOG.md bin/main.ml
git commit -m "chore: changelog and version bump to 0.2.0"

git tag -a v0.2.0 -m "v0.2.0: schema-aware tables, cell-level merge, UUIDv7, new types"
git push origin congruence
git push origin v0.2.0
```
