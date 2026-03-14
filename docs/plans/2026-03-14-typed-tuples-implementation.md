# Typed Tuples Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement order-preserving tuple encoding (Int64, String) and Db.range, enabling composite primary keys that sort correctly under byte comparison.

**Architecture:** New `Tuple` module with encode/decode using type-tagged, order-preserving binary format. Add `Db.range` as thin wrapper over `Tree.range`. Two new acceptance tests verify numeric and composite key ordering through the full stack.

**Tech Stack:** OCaml, Alcotest, dune.

**Environment:** Run all commands inside `guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && ...'`. Use `ALCOTEST_COLOR=never` for readable test output.

**Design doc:** `docs/plans/2026-03-14-typed-tuples-design.md`

---

### Task 1: Tuple module with unit tests

**Files:**
- Create: `lib/tuple.mli`
- Create: `lib/tuple.ml`
- Create: `test/test_tuple.ml`
- Modify: `lib/bole.ml` (add `module Tuple = Tuple`)
- Modify: `test/test_main.ml` (add `Test_tuple.tests`)

**Step 1: Create the interface**

Create `lib/tuple.mli`:

```ocaml
(** Order-preserving tuple encoding.

    Encodes typed value lists into bytes such that byte comparison
    (String.compare) gives the correct sort order. Used for prolly
    tree keys that need numeric or composite ordering.

    Encoding format: each value is type-tagged.
    - Int64 (tag 0x01): 8 bytes big-endian with sign-bit flip
    - String (tag 0x02): 2-byte BE length + bytes + 0x00 terminator *)

type value =
  | Int64 of int64
  | String of string

type t = value list

val encode : t -> string
val decode : string -> t
```

**Step 2: Implement**

Create `lib/tuple.ml`:

```ocaml
type value =
  | Int64 of int64
  | String of string

type t = value list

let tag_int64 = '\x01'
let tag_string = '\x02'

let encode values =
  let buf = Buffer.create 64 in
  List.iter (fun v ->
    match v with
    | Int64 n ->
      Buffer.add_char buf tag_int64;
      (* Big-endian with sign-bit flip for order-preserving encoding *)
      let bits = Int64.to_int n in
      let flipped = Int64.logxor n Int64.min_int in
      for i = 7 downto 0 do
        let byte = Int64.to_int (Int64.shift_right_logical flipped (i * 8)) land 0xFF in
        Buffer.add_char buf (Char.chr byte)
      done;
      ignore bits
    | String s ->
      Buffer.add_char buf tag_string;
      let len = String.length s in
      Buffer.add_char buf (Char.chr (len lsr 8 land 0xFF));
      Buffer.add_char buf (Char.chr (len land 0xFF));
      Buffer.add_string buf s;
      Buffer.add_char buf '\x00'
  ) values;
  Buffer.contents buf

let decode data =
  let pos = ref 0 in
  let len = String.length data in
  let values = ref [] in
  while !pos < len do
    let tag = data.[!pos] in
    pos := !pos + 1;
    match tag with
    | c when c = tag_int64 ->
      let flipped = ref 0L in
      for i = 0 to 7 do
        let byte = Char.code data.[!pos + i] in
        flipped := Int64.logor (Int64.shift_left !flipped 8) (Int64.of_int byte)
      done;
      pos := !pos + 8;
      let n = Int64.logxor !flipped Int64.min_int in
      values := Int64 n :: !values
    | c when c = tag_string ->
      let hi = Char.code data.[!pos] in
      let lo = Char.code data.[!pos + 1] in
      let slen = (hi lsl 8) lor lo in
      pos := !pos + 2;
      let s = String.sub data !pos slen in
      pos := !pos + slen + 1; (* +1 for null terminator *)
      values := String s :: !values
    | _ -> invalid_arg (Printf.sprintf "Tuple.decode: unknown tag 0x%02x" (Char.code tag))
  done;
  List.rev !values
```

Note: The `encode` for Int64 has a bug — `let bits = Int64.to_int n in` is unused and wrong. The correct implementation uses `flipped` directly. Here's the corrected Int64 encoding:

```ocaml
    | Int64 n ->
      Buffer.add_char buf tag_int64;
      let flipped = Int64.logxor n Int64.min_int in
      for i = 7 downto 0 do
        let byte = Int64.to_int (Int64.shift_right_logical flipped (i * 8)) land 0xFF in
        Buffer.add_char buf (Char.chr byte)
      done
```

**Step 3: Write unit tests**

Create `test/test_tuple.ml`:

```ocaml
let test_int64_round_trip () =
  let values = [Bole.Tuple.Int64 42L] in
  let encoded = Bole.Tuple.encode values in
  let decoded = Bole.Tuple.decode encoded in
  Alcotest.(check int) "one value" 1 (List.length decoded);
  match decoded with
  | [Bole.Tuple.Int64 42L] -> ()
  | _ -> Alcotest.fail "expected Int64 42"

let test_string_round_trip () =
  let values = [Bole.Tuple.String "hello"] in
  let decoded = Bole.Tuple.decode (Bole.Tuple.encode values) in
  match decoded with
  | [Bole.Tuple.String "hello"] -> ()
  | _ -> Alcotest.fail "expected String hello"

let test_multi_field_round_trip () =
  let values = [Bole.Tuple.String "alice"; Bole.Tuple.Int64 30L; Bole.Tuple.String "admin"] in
  let decoded = Bole.Tuple.decode (Bole.Tuple.encode values) in
  Alcotest.(check int) "three fields" 3 (List.length decoded);
  match decoded with
  | [Bole.Tuple.String "alice"; Bole.Tuple.Int64 30L; Bole.Tuple.String "admin"] -> ()
  | _ -> Alcotest.fail "unexpected decoded values"

let test_empty_round_trip () =
  let encoded = Bole.Tuple.encode [] in
  let decoded = Bole.Tuple.decode encoded in
  Alcotest.(check int) "empty" 0 (List.length decoded)

let test_int64_ordering () =
  let encode_one n = Bole.Tuple.encode [Bole.Tuple.Int64 n] in
  let a = encode_one Int64.min_int in
  let b = encode_one (-1L) in
  let c = encode_one 0L in
  let d = encode_one 1L in
  let e = encode_one 100L in
  let f = encode_one Int64.max_int in
  Alcotest.(check bool) "min < -1" true (String.compare a b < 0);
  Alcotest.(check bool) "-1 < 0" true (String.compare b c < 0);
  Alcotest.(check bool) "0 < 1" true (String.compare c d < 0);
  Alcotest.(check bool) "1 < 100" true (String.compare d e < 0);
  Alcotest.(check bool) "100 < max" true (String.compare e f < 0)

let test_string_ordering () =
  let encode_one s = Bole.Tuple.encode [Bole.Tuple.String s] in
  let a = encode_one "" in
  let b = encode_one "ab" in
  let c = encode_one "abc" in
  let d = encode_one "b" in
  Alcotest.(check bool) "empty < ab" true (String.compare a b < 0);
  Alcotest.(check bool) "ab < abc" true (String.compare b c < 0);
  Alcotest.(check bool) "abc < b" true (String.compare c d < 0)

let test_composite_ordering () =
  let encode_pair s n = Bole.Tuple.encode [Bole.Tuple.String s; Bole.Tuple.Int64 n] in
  let a = encode_pair "alice" 10L in
  let b = encode_pair "alice" 20L in
  let c = encode_pair "bob" 1L in
  Alcotest.(check bool) "alice,10 < alice,20" true (String.compare a b < 0);
  Alcotest.(check bool) "alice,20 < bob,1" true (String.compare b c < 0)

let tests =
  [ "tuple", [
      Alcotest.test_case "int64 round-trip" `Quick test_int64_round_trip;
      Alcotest.test_case "string round-trip" `Quick test_string_round_trip;
      Alcotest.test_case "multi-field round-trip" `Quick test_multi_field_round_trip;
      Alcotest.test_case "empty round-trip" `Quick test_empty_round_trip;
      Alcotest.test_case "int64 ordering" `Quick test_int64_ordering;
      Alcotest.test_case "string ordering" `Quick test_string_ordering;
      Alcotest.test_case "composite ordering" `Quick test_composite_ordering;
    ]
  ]
```

**Step 4: Wire up**

Add `module Tuple = Tuple` to `lib/bole.ml` (before `module Db = Db`).

Add `Test_tuple.tests` to `test/test_main.ml` (after `Test_merge.tests`, before `Test_acceptance.tests`).

**Step 5: Run tests and commit**

Run: `guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && ALCOTEST_COLOR=never dune test'`

Expected: All existing 104 tests pass, plus 7 new tuple tests = 111 total.

```bash
git add lib/tuple.mli lib/tuple.ml lib/bole.ml test/test_tuple.ml test/test_main.ml
git commit -m "feat: add Tuple module with order-preserving encoding"
```

---

### Task 2: Db.range

**Files:**
- Modify: `lib/db.mli`
- Modify: `lib/db.ml`

**Step 1: Add to interface**

Add to `lib/db.mli`:

```ocaml
val range : t -> table:string -> (string * string) Seq.t
```

**Step 2: Implement**

Add to `lib/db.ml`:

```ocaml
let range db ~table =
  match StringMap.find_opt table db.tables with
  | None -> Seq.empty
  | Some root -> Tree.range db.store root
```

**Step 3: Run tests and commit**

All existing tests pass.

```bash
git add lib/db.mli lib/db.ml
git commit -m "feat: add Db.range for table-level range scans"
```

---

### Task 3: Acceptance tests for typed key ordering

**Files:**
- Modify: `test/test_acceptance.ml`

**Step 1: Add acceptance tests**

Add to `test/test_acceptance.ml`, before the `tests` registration list:

```ocaml
(* --- Test 8: Int64 key ordering --- *)

let test_int64_key_ordering () =
  let db = Bole.Db.create () in
  let put db n v =
    Bole.Db.put db ~table:"scores"
      ~key:(Bole.Tuple.encode [Bole.Tuple.Int64 n])
      ~value:v
  in
  let db = put db 9L "nine" in
  let db = put db 10L "ten" in
  let db = put db 2L "two" in
  let db = put db 100L "hundred" in
  let db = put db (-1L) "neg-one" in
  let db = put db 0L "zero" in
  let all = Bole.Db.range db ~table:"scores" |> List.of_seq in
  let values = List.map snd all in
  Alcotest.(check (list string)) "numeric order"
    ["neg-one"; "zero"; "two"; "nine"; "ten"; "hundred"] values

(* --- Test 9: Composite key ordering --- *)

let test_composite_key_ordering () =
  let db = Bole.Db.create () in
  let put db s n v =
    Bole.Db.put db ~table:"t"
      ~key:(Bole.Tuple.encode [Bole.Tuple.String s; Bole.Tuple.Int64 n])
      ~value:v
  in
  let db = put db "bob" 2L "bob-2" in
  let db = put db "alice" 10L "alice-10" in
  let db = put db "alice" 2L "alice-2" in
  let db = put db "bob" 1L "bob-1" in
  let all = Bole.Db.range db ~table:"t" |> List.of_seq in
  let values = List.map snd all in
  Alcotest.(check (list string)) "composite order"
    ["alice-2"; "alice-10"; "bob-1"; "bob-2"] values
```

Register them in the `tests` list:

```ocaml
let tests =
  [ "acceptance", [
      Alcotest.test_case "basic table operations" `Quick test_basic_table_workflow;
      Alcotest.test_case "commit and history" `Quick test_commit_and_history;
      Alcotest.test_case "branching" `Quick test_branching;
      Alcotest.test_case "diff between commits" `Quick test_diff_branches;
      Alcotest.test_case "clean merge" `Quick test_clean_merge;
      Alcotest.test_case "conflicting merge" `Quick test_conflicting_merge;
      Alcotest.test_case "multi-table commits" `Quick test_multi_table_commit;
      Alcotest.test_case "int64 key ordering" `Quick test_int64_key_ordering;
      Alcotest.test_case "composite key ordering" `Quick test_composite_key_ordering;
    ]
  ]
```

**Step 2: Run tests**

Run: `guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && ALCOTEST_COLOR=never dune test'`

Expected: All 113 tests pass (111 + 2 new acceptance tests).

**Step 3: Commit**

```bash
git add test/test_acceptance.ml
git commit -m "test: add acceptance tests for typed key ordering"
```
