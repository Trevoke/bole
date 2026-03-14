# Step 9: Performance Property Tests Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add Store instrumentation (get/put counters) and two performance tests verifying structural sharing and diff efficiency — the core algorithmic guarantees of prolly trees.

**Architecture:** Add `get_count`, `put_count`, `reset_stats` to the Store module via mutable int ref fields. Create `test/test_perf.ml` with two `Slow`-tagged tests that build 1000-key trees and assert O(log n) / O(d log n) bounds.

**Tech Stack:** OCaml, Alcotest, dune.

**Environment:** Run all commands inside `guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && ...'`. Use `ALCOTEST_COLOR=never` for readable test output.

**Design doc:** `docs/plans/2026-03-14-step-9-design.md`

---

### Task 1: Store instrumentation

**Files:**
- Modify: `lib/store.mli`
- Modify: `lib/store.ml`
- Modify: `test/test_store.ml`

**Step 1: Write the failing test**

Add to `test/test_store.ml`, before the `tests` list:

```ocaml
let test_stats () =
  let store = Bole.Store.create () in
  Alcotest.(check int) "initial get_count" 0 (Bole.Store.get_count store);
  Alcotest.(check int) "initial put_count" 0 (Bole.Store.put_count store);
  let h = Bole.Store.put store "data" in
  Alcotest.(check int) "put_count after put" 1 (Bole.Store.put_count store);
  let _ = Bole.Store.get store h in
  Alcotest.(check int) "get_count after get" 1 (Bole.Store.get_count store);
  let _ = Bole.Store.get store h in
  Alcotest.(check int) "get_count after second get" 2 (Bole.Store.get_count store);
  Bole.Store.reset_stats store;
  Alcotest.(check int) "get_count after reset" 0 (Bole.Store.get_count store);
  Alcotest.(check int) "put_count after reset" 0 (Bole.Store.put_count store)
```

Register it in the `tests` list:

```ocaml
let tests =
  [ "store", [
      Alcotest.test_case "put/get round-trip" `Quick test_put_get_round_trip;
      Alcotest.test_case "put idempotent" `Quick test_put_idempotent;
      Alcotest.test_case "mem after put" `Quick test_mem_after_put;
      Alcotest.test_case "mem unknown" `Quick test_mem_unknown;
      Alcotest.test_case "get unknown raises" `Quick test_get_unknown_raises;
      QCheck_alcotest.to_alcotest prop_round_trip;
      QCheck_alcotest.to_alcotest prop_content_addressing;
      Alcotest.test_case "stats" `Quick test_stats;
    ]
  ]
```

**Step 2: Run tests to verify they fail**

Run: `guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && ALCOTEST_COLOR=never dune test' 2>&1`

Expected: Compilation error — `Bole.Store.get_count` does not exist.

**Step 3: Add the interface**

Add to `lib/store.mli`, after the `mem` declaration:

```ocaml
(** [get_count store] returns the number of [get] calls since creation
    or the last [reset_stats]. *)
val get_count : t -> int

(** [put_count store] returns the number of [put] calls since creation
    or the last [reset_stats]. *)
val put_count : t -> int

(** [reset_stats store] resets [get_count] and [put_count] to zero. *)
val reset_stats : t -> unit
```

**Step 4: Implement the counters**

Modify `lib/store.ml`. Change the record type and all functions:

```ocaml
module Tbl = Hashtbl.Make (struct
  type t = Hash.t
  let equal = Hash.equal
  let hash h = Hashtbl.hash (Hash.to_raw_string h)
end)

type t = {
  tbl : string Tbl.t;
  mutable gets : int;
  mutable puts : int;
}

let create () = { tbl = Tbl.create 1024; gets = 0; puts = 0 }

let put store data =
  store.puts <- store.puts + 1;
  let h = Hash.hash data in
  if not (Tbl.mem store.tbl h) then
    Tbl.replace store.tbl h data;
  h

let get store h =
  store.gets <- store.gets + 1;
  Tbl.find store.tbl h

let mem store h =
  Tbl.mem store.tbl h

let get_count store = store.gets
let put_count store = store.puts
let reset_stats store =
  store.gets <- 0;
  store.puts <- 0
```

**Step 5: Run tests to verify they pass**

Run: `guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && ALCOTEST_COLOR=never dune test' 2>&1`

Expected: All tests pass (75 tests — 74 existing + 1 new).

**Step 6: Commit**

```bash
git add lib/store.mli lib/store.ml test/test_store.ml
git commit -m "feat: add get/put counters to Store for performance testing"
```

---

### Task 2: Structural sharing test

**Files:**
- Create: `test/test_perf.ml`
- Modify: `test/test_main.ml`

**Step 1: Write the test**

Create `test/test_perf.ml`:

```ocaml
let test_structural_sharing () =
  let store = Bole.Store.create () in
  let pairs = List.init 1000 (fun i ->
    (Printf.sprintf "key-%06d" i, Printf.sprintf "val-%06d" i)) in
  let root = Bole.Tree.build ~target_size:20 store (List.to_seq pairs) in
  (* Reset stats after build *)
  Bole.Store.reset_stats store;
  (* Put one new key *)
  let _root' = Bole.Tree.put ~target_size:20 store root "key-000500a" "new-value" in
  let new_chunks = Bole.Store.put_count store in
  (* Tree height for 1000 keys with target_size:20 is ~3.
     A single put should create O(log n) new chunks:
     1 modified leaf + ancestors + possible splits.
     Assert <= 15 (generous bound). *)
  Alcotest.(check bool)
    (Printf.sprintf "structural sharing: %d new chunks <= 15" new_chunks)
    true (new_chunks <= 15);
  (* Also verify it's not zero — something should have changed *)
  Alcotest.(check bool) "at least 1 new chunk" true (new_chunks >= 1);
  (* Verify the old tree is still intact *)
  Alcotest.(check (option string)) "old tree unchanged"
    (Some "val-00500") (Bole.Tree.find store root "key-000500")

let tests =
  [ "perf", [
      Alcotest.test_case "structural sharing" `Slow test_structural_sharing;
    ]
  ]
```

**Step 2: Wire up the test runner**

Modify `test/test_main.ml` — add `Test_perf.tests` to the `List.concat`:

```ocaml
let () =
  Alcotest.run "bole"
    (List.concat
       [ Test_hash.tests
       ; Test_store.tests
       ; Test_chunk.tests
       ; Test_chunker.tests
       ; Test_tree.tests
       ; Test_diff.tests
       ; Test_perf.tests
       ])
```

**Step 3: Run tests to verify they pass**

Run: `guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && ALCOTEST_COLOR=never dune test' 2>&1`

Expected: All tests pass (76 tests — 75 existing + 1 new).

**Step 4: Commit**

```bash
git add test/test_perf.ml test/test_main.ml
git commit -m "test: add structural sharing performance test"
```

---

### Task 3: Diff efficiency test

**Files:**
- Modify: `test/test_perf.ml`

**Step 1: Write the test**

Add to `test/test_perf.ml`, before the `tests` list:

```ocaml
let test_diff_efficiency () =
  let store = Bole.Store.create () in
  let pairs = List.init 1000 (fun i ->
    (Printf.sprintf "key-%06d" i, Printf.sprintf "val-%06d" i)) in
  let root_a = Bole.Tree.build ~target_size:20 store (List.to_seq pairs) in
  (* Apply 5 mutations spread across the key range *)
  let root_b = Bole.Tree.put ~target_size:20 store root_a "key-000100" "changed-1" in
  let root_b = Bole.Tree.put ~target_size:20 store root_b "key-000300" "changed-2" in
  let root_b = Bole.Tree.put ~target_size:20 store root_b "key-000500" "changed-3" in
  let root_b = Bole.Tree.put ~target_size:20 store root_b "key-000700" "changed-4" in
  let root_b = Bole.Tree.put ~target_size:20 store root_b "key-000900" "changed-5" in
  (* Measure full traversal cost *)
  Bole.Store.reset_stats store;
  Bole.Tree.range store root_a |> Seq.iter (fun _ -> ());
  let full_gets = Bole.Store.get_count store in
  (* Measure diff cost *)
  Bole.Store.reset_stats store;
  Bole.Diff.diff store ~from:root_a ~to_:root_b |> Seq.iter (fun _ -> ());
  let diff_gets = Bole.Store.get_count store in
  (* Diff should read significantly fewer chunks than full traversal.
     With 5 changes in 1000 keys, most subtrees are shared and skipped. *)
  Alcotest.(check bool)
    (Printf.sprintf "diff efficiency: %d diff gets < %d full gets / 2"
       diff_gets full_gets)
    true (diff_gets < full_gets / 2);
  (* Sanity: diff should have read at least something *)
  Alcotest.(check bool) "diff read at least 1 chunk" true (diff_gets >= 1)
```

Register it in the `tests` list:

```ocaml
let tests =
  [ "perf", [
      Alcotest.test_case "structural sharing" `Slow test_structural_sharing;
      Alcotest.test_case "diff efficiency" `Slow test_diff_efficiency;
    ]
  ]
```

**Step 2: Run tests to verify they pass**

Run: `guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && ALCOTEST_COLOR=never dune test' 2>&1`

Expected: All tests pass (77 tests — 75 existing + 2 new).

**Step 3: Commit**

```bash
git add test/test_perf.ml
git commit -m "test: add diff efficiency performance test"
```
