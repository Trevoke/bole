# Step 8: Diff (Parallel Descent) Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement a `Diff` module that computes differences between two prolly trees via parallel recursive descent, returning a lazy `Seq.t` of diff entries in O(d log n) time.

**Architecture:** A new `Diff` module with one type (`entry`) and one function (`diff`). The function performs recursive parallel descent of two trees, merge-joining entries at each level. Equal child hashes are skipped entirely (the source of O(d log n)). One-sided subtrees are flattened via `Tree.range`. The result is a lazy `entry Seq.t` emitted in key order.

**Tech Stack:** OCaml, Alcotest, QCheck2. Depends on `Store`, `Hash`, `Chunk`, `Tree` modules.

**Environment:** Run all commands inside `guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && ...'`. Use `ALCOTEST_COLOR=never` for readable test output.

**Design doc:** `docs/plans/2026-03-13-step-8-design.md`

---

### Task 1: Diff module skeleton with identical-trees test

**Files:**
- Create: `lib/diff.mli`
- Create: `lib/diff.ml`
- Modify: `lib/bole.ml`
- Create: `test/test_diff.ml`
- Modify: `test/test_main.ml`

**Step 1: Write the test file with the first test**

Create `test/test_diff.ml`:

```ocaml
let test_identical_trees () =
  let store = Bole.Store.create () in
  let pairs = List.init 10 (fun i ->
    (Printf.sprintf "key-%03d" i, Printf.sprintf "val-%03d" i)) in
  let root = Bole.Tree.build store (List.to_seq pairs) in
  let entries = Bole.Diff.diff store ~from:root ~to_:root |> List.of_seq in
  Alcotest.(check int) "no differences" 0 (List.length entries)

let tests =
  [ "diff", [
      Alcotest.test_case "identical trees" `Quick test_identical_trees;
    ]
  ]
```

**Step 2: Wire up the test runner**

Modify `test/test_main.ml` — add `Test_diff.tests` to the `List.concat`:

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
       ])
```

**Step 3: Run tests to verify they fail**

Run: `guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && ALCOTEST_COLOR=never dune test' 2>&1`

Expected: Compilation error — `Bole.Diff` does not exist.

**Step 4: Create the interface**

Create `lib/diff.mli`:

```ocaml
(** Compute differences between two prolly trees.

    [diff store ~from ~to_] returns a lazy sequence of differences
    between the tree rooted at [from] (old) and the tree rooted at
    [to_] (new). Entries are emitted in key order.

    - [Removed(key, value)]: present in [from] but absent in [to_]
    - [Added(key, value)]: present in [to_] but absent in [from]
    - [Modified(key, old_value, new_value)]: key exists in both with different values

    Skips shared subtrees by comparing hashes, achieving O(d log n)
    where d is the number of differing entries. *)

type entry =
  | Added of string * string
  | Removed of string * string
  | Modified of string * string * string

val diff : Store.t -> from:Hash.t -> to_:Hash.t -> entry Seq.t
```

**Step 5: Create a minimal implementation**

Create `lib/diff.ml`:

```ocaml
type entry =
  | Added of string * string
  | Removed of string * string
  | Modified of string * string * string

let diff store ~from ~to_ =
  if Hash.equal from to_ then Seq.empty
  else
    (* Placeholder: flatten both trees and merge-join *)
    let _ = store in
    let _ = from in
    let _ = to_ in
    Seq.empty
```

**Step 6: Register the module in bole.ml**

Modify `lib/bole.ml` — add `module Diff = Diff` at the end:

```ocaml
module Hash = Hash
module Store = Store
module Chunk = Chunk
module Chunker = Chunker
module Tree = Tree
module Diff = Diff
```

**Step 7: Run tests to verify they pass**

Run: `guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && ALCOTEST_COLOR=never dune test' 2>&1`

Expected: All tests pass (64 tests — 63 existing + 1 new).

**Step 8: Commit**

```bash
git add lib/diff.mli lib/diff.ml lib/bole.ml test/test_diff.ml test/test_main.ml
git commit -m "feat: add Diff module skeleton with identical-trees test"
```

---

### Task 2: Leaf merge-join

**Files:**
- Modify: `test/test_diff.ml`
- Modify: `lib/diff.ml`

**Step 1: Write failing tests for leaf-level diffs**

Add to `test/test_diff.ml`, before the `tests` list:

```ocaml
let test_empty_vs_nonempty () =
  let store = Bole.Store.create () in
  let empty = Bole.Tree.build store Seq.empty in
  let pairs = List.init 10 (fun i ->
    (Printf.sprintf "key-%03d" i, Printf.sprintf "val-%03d" i)) in
  let full = Bole.Tree.build store (List.to_seq pairs) in
  let added = Bole.Diff.diff store ~from:empty ~to_:full |> List.of_seq in
  Alcotest.(check int) "10 additions" 10 (List.length added);
  List.iter (fun e ->
    match e with
    | Bole.Diff.Added _ -> ()
    | _ -> Alcotest.fail "expected Added"
  ) added;
  let removed = Bole.Diff.diff store ~from:full ~to_:empty |> List.of_seq in
  Alcotest.(check int) "10 removals" 10 (List.length removed);
  List.iter (fun e ->
    match e with
    | Bole.Diff.Removed _ -> ()
    | _ -> Alcotest.fail "expected Removed"
  ) removed

let test_single_addition () =
  let store = Bole.Store.create () in
  let pairs = [("a", "1"); ("b", "2"); ("c", "3")] in
  let root = Bole.Tree.build store (List.to_seq pairs) in
  let root' = Bole.Tree.put store root "d" "4" in
  let entries = Bole.Diff.diff store ~from:root ~to_:root' |> List.of_seq in
  Alcotest.(check int) "one diff entry" 1 (List.length entries);
  match entries with
  | [Bole.Diff.Added ("d", "4")] -> ()
  | _ -> Alcotest.fail "expected Added(d, 4)"

let test_single_deletion () =
  let store = Bole.Store.create () in
  let pairs = [("a", "1"); ("b", "2"); ("c", "3")] in
  let root = Bole.Tree.build store (List.to_seq pairs) in
  let root' = Bole.Tree.delete store root "b" in
  let entries = Bole.Diff.diff store ~from:root ~to_:root' |> List.of_seq in
  Alcotest.(check int) "one diff entry" 1 (List.length entries);
  match entries with
  | [Bole.Diff.Removed ("b", "2")] -> ()
  | _ -> Alcotest.fail "expected Removed(b, 2)"

let test_single_modification () =
  let store = Bole.Store.create () in
  let pairs = [("a", "1"); ("b", "2"); ("c", "3")] in
  let root = Bole.Tree.build store (List.to_seq pairs) in
  let root' = Bole.Tree.put store root "b" "99" in
  let entries = Bole.Diff.diff store ~from:root ~to_:root' |> List.of_seq in
  Alcotest.(check int) "one diff entry" 1 (List.length entries);
  match entries with
  | [Bole.Diff.Modified ("b", "2", "99")] -> ()
  | _ -> Alcotest.fail "expected Modified(b, 2, 99)"
```

Register them in the `tests` list:

```ocaml
let tests =
  [ "diff", [
      Alcotest.test_case "identical trees" `Quick test_identical_trees;
      Alcotest.test_case "empty vs non-empty" `Quick test_empty_vs_nonempty;
      Alcotest.test_case "single addition" `Quick test_single_addition;
      Alcotest.test_case "single deletion" `Quick test_single_deletion;
      Alcotest.test_case "single modification" `Quick test_single_modification;
    ]
  ]
```

**Step 2: Run tests to verify they fail**

Run: `guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && ALCOTEST_COLOR=never dune test' 2>&1`

Expected: Tests fail — diff returns empty seq for non-identical trees.

**Step 3: Implement leaf merge-join and full diff_nodes**

Replace the contents of `lib/diff.ml` with:

```ocaml
type entry =
  | Added of string * string
  | Removed of string * string
  | Modified of string * string * string

let rec merge_leaves left right () =
  match left, right with
  | [], [] -> Seq.Nil
  | [], (e : Chunk.leaf_entry) :: rest ->
    Seq.Cons (Added (e.key, e.value), merge_leaves [] rest)
  | (e : Chunk.leaf_entry) :: rest, [] ->
    Seq.Cons (Removed (e.key, e.value), merge_leaves rest [])
  | (l : Chunk.leaf_entry) :: ls, (r : Chunk.leaf_entry) :: rs ->
    let cmp = String.compare l.key r.key in
    if cmp < 0 then
      Seq.Cons (Removed (l.key, l.value), merge_leaves ls right)
    else if cmp > 0 then
      Seq.Cons (Added (r.key, r.value), merge_leaves left rs)
    else if l.value = r.value then
      merge_leaves ls rs ()
    else
      Seq.Cons (Modified (l.key, l.value, r.value), merge_leaves ls rs)

let emit_all_as tag store h =
  Tree.range store h
  |> Seq.map (fun (k, v) ->
    match tag with
    | `Added -> Added (k, v)
    | `Removed -> Removed (k, v))

let seq_append s1 s2 =
  let rec go s1 s2 () =
    match s1 () with
    | Seq.Nil -> s2 ()
    | Seq.Cons (x, rest) -> Seq.Cons (x, go rest s2)
  in
  go s1 s2

let rec diff_nodes store h1 h2 =
  if Hash.equal h1 h2 then Seq.empty
  else
    let c1 = Chunk.decode (Store.get store h1) in
    let c2 = Chunk.decode (Store.get store h2) in
    match c1, c2 with
    | Chunk.Leaf l1, Chunk.Leaf l2 ->
      merge_leaves l1 l2
    | Chunk.Internal e1, Chunk.Internal e2 ->
      merge_internals store e1 e2
    | _ ->
      (* Mixed types: flatten both to leaves via range *)
      let left = Tree.range store h1 |> List.of_seq in
      let right = Tree.range store h2 |> List.of_seq in
      let to_leaf (k, v) = ({ Chunk.key = k; value = v } : Chunk.leaf_entry) in
      merge_leaves (List.map to_leaf left) (List.map to_leaf right)

and merge_internals store left right =
  let rec go left right () =
    match left, right with
    | [], [] -> Seq.Nil
    | [], (e : Chunk.internal_entry) :: rest ->
      let added = emit_all_as `Added store e.child in
      seq_append added (go [] rest) ()
    | (e : Chunk.internal_entry) :: rest, [] ->
      let removed = emit_all_as `Removed store e.child in
      seq_append removed (go rest []) ()
    | (l : Chunk.internal_entry) :: ls, (r : Chunk.internal_entry) :: rs ->
      let cmp = String.compare l.key r.key in
      if cmp < 0 then
        let removed = emit_all_as `Removed store l.child in
        seq_append removed (go ls right) ()
      else if cmp > 0 then
        let added = emit_all_as `Added store r.child in
        seq_append added (go left rs) ()
      else
        let child_diff = diff_nodes store l.child r.child in
        seq_append child_diff (go ls rs) ()
  in
  go left right

let diff store ~from ~to_ =
  if Hash.equal from to_ then Seq.empty
  else diff_nodes store from to_
```

**Step 4: Run tests to verify they pass**

Run: `guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && ALCOTEST_COLOR=never dune test' 2>&1`

Expected: All tests pass (68 tests — 63 existing + 5 new).

**Step 5: Commit**

```bash
git add lib/diff.ml test/test_diff.ml
git commit -m "feat: implement diff with leaf merge-join and parallel descent"
```

---

### Task 3: Multi-chunk and key-ordering tests

**Files:**
- Modify: `test/test_diff.ml`

**Step 1: Write tests for multiple changes and multi-chunk trees**

Add to `test/test_diff.ml`, before the `tests` list:

```ocaml
let test_multiple_changes () =
  let store = Bole.Store.create () in
  let pairs = [("a", "1"); ("b", "2"); ("c", "3"); ("d", "4"); ("e", "5")] in
  let root = Bole.Tree.build store (List.to_seq pairs) in
  let root' = Bole.Tree.put store root "b" "20" in       (* modify *)
  let root' = Bole.Tree.delete store root' "d" in         (* delete *)
  let root' = Bole.Tree.put store root' "cc" "new" in     (* add *)
  let entries = Bole.Diff.diff store ~from:root ~to_:root' |> List.of_seq in
  (* Expect in key order: Modified b, Added cc, Removed d *)
  Alcotest.(check int) "three diff entries" 3 (List.length entries);
  let keys = List.map (fun e ->
    match e with
    | Bole.Diff.Added (k, _) -> k
    | Bole.Diff.Removed (k, _) -> k
    | Bole.Diff.Modified (k, _, _) -> k
  ) entries in
  Alcotest.(check (list string)) "keys in order" ["b"; "cc"; "d"] keys;
  (match List.nth entries 0 with
   | Bole.Diff.Modified ("b", "2", "20") -> ()
   | _ -> Alcotest.fail "expected Modified(b)");
  (match List.nth entries 1 with
   | Bole.Diff.Added ("cc", "new") -> ()
   | _ -> Alcotest.fail "expected Added(cc)");
  (match List.nth entries 2 with
   | Bole.Diff.Removed ("d", "4") -> ()
   | _ -> Alcotest.fail "expected Removed(d)")

let test_multi_chunk_diff () =
  let store = Bole.Store.create () in
  let pairs = List.init 200 (fun i ->
    (Printf.sprintf "key-%05d" i, Printf.sprintf "val-%05d" i)) in
  let root = Bole.Tree.build ~target_size:20 store (List.to_seq pairs) in
  (* Apply several mutations *)
  let root' = Bole.Tree.put ~target_size:20 store root "key-00050" "changed" in
  let root' = Bole.Tree.delete ~target_size:20 store root' "key-00100" in
  let root' = Bole.Tree.put ~target_size:20 store root' "key-00200" "new-entry" in
  let entries = Bole.Diff.diff store ~from:root ~to_:root' |> List.of_seq in
  Alcotest.(check int) "three diff entries" 3 (List.length entries);
  (* Verify each entry *)
  let has e = List.mem e entries in
  Alcotest.(check bool) "modified key-00050"
    true (has (Bole.Diff.Modified ("key-00050", "val-00050", "changed")));
  Alcotest.(check bool) "removed key-00100"
    true (has (Bole.Diff.Removed ("key-00100", "val-00100")));
  Alcotest.(check bool) "added key-00200"
    true (has (Bole.Diff.Added ("key-00200", "new-entry")));
  (* Verify key ordering *)
  let keys = List.map (fun e ->
    match e with
    | Bole.Diff.Added (k, _) -> k
    | Bole.Diff.Removed (k, _) -> k
    | Bole.Diff.Modified (k, _, _) -> k
  ) entries in
  let sorted = List.sort String.compare keys in
  Alcotest.(check (list string)) "keys in sorted order" sorted keys

let test_key_ordering () =
  let store = Bole.Store.create () in
  let pairs = List.init 50 (fun i ->
    (Printf.sprintf "k-%03d" i, Printf.sprintf "v-%03d" i)) in
  let root = Bole.Tree.build ~target_size:20 store (List.to_seq pairs) in
  let empty = Bole.Tree.build store Seq.empty in
  let entries = Bole.Diff.diff store ~from:empty ~to_:root |> List.of_seq in
  let keys = List.map (fun e ->
    match e with
    | Bole.Diff.Added (k, _) -> k
    | _ -> Alcotest.fail "expected Added"
  ) entries in
  let sorted = List.sort String.compare keys in
  Alcotest.(check (list string)) "all entries in key order" sorted keys
```

Register them in the `tests` list:

```ocaml
let tests =
  [ "diff", [
      Alcotest.test_case "identical trees" `Quick test_identical_trees;
      Alcotest.test_case "empty vs non-empty" `Quick test_empty_vs_nonempty;
      Alcotest.test_case "single addition" `Quick test_single_addition;
      Alcotest.test_case "single deletion" `Quick test_single_deletion;
      Alcotest.test_case "single modification" `Quick test_single_modification;
      Alcotest.test_case "multiple changes" `Quick test_multiple_changes;
      Alcotest.test_case "multi-chunk diff" `Quick test_multi_chunk_diff;
      Alcotest.test_case "key ordering" `Quick test_key_ordering;
    ]
  ]
```

**Step 2: Run tests to verify they pass**

Run: `guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && ALCOTEST_COLOR=never dune test' 2>&1`

Expected: All tests pass (71 tests — 63 existing + 8 new).

Note: If any fail, debug the merge-join logic in `diff.ml`. The most likely issue is incorrect cursor advancement in `merge_internals` when internal entries have overlapping key ranges.

**Step 3: Commit**

```bash
git add test/test_diff.ml
git commit -m "test: add multi-chunk and key-ordering diff tests"
```

---

### Task 4: QCheck property tests

**Files:**
- Modify: `test/test_diff.ml`

**Step 1: Write the three QCheck property tests**

Add to `test/test_diff.ml`, before the `tests` list:

```ocaml
let diff_entry_key = function
  | Bole.Diff.Added (k, _) -> k
  | Bole.Diff.Removed (k, _) -> k
  | Bole.Diff.Modified (k, _, _) -> k

let prop_diff_completeness =
  QCheck2.Test.make ~name:"diff captures exactly the applied changes"
    ~count:20
    QCheck2.Gen.(pair
      (list_size (int_range 10 80)
        (pair
          (string_size ~gen:printable (int_range 1 10))
          (string_size ~gen:printable (int_range 1 10))))
      (list_size (int_range 1 20)
        (pair
          (string_size ~gen:printable (int_range 1 10))
          (string_size ~gen:printable (int_range 1 10)))))
    (fun (initial_pairs, mutations) ->
       let sorted = List.sort_uniq (fun (k1, _) (k2, _) ->
         String.compare k1 k2) initial_pairs in
       if List.length sorted < 5 then true
       else begin
         let store = Bole.Store.create () in
         let root_a = Bole.Tree.build ~target_size:20 store (List.to_seq sorted) in
         (* Apply mutations: treat each as a put *)
         let root_b = List.fold_left (fun r (k, v) ->
           Bole.Tree.put ~target_size:20 store r k v
         ) root_a mutations in
         let diff_entries = Bole.Diff.diff store ~from:root_a ~to_:root_b
           |> List.of_seq in
         (* Every diff entry should reflect a real difference *)
         List.for_all (fun e ->
           match e with
           | Bole.Diff.Added (k, v) ->
             Bole.Tree.find store root_a k = None
             && Bole.Tree.find store root_b k = Some v
           | Bole.Diff.Removed (k, v) ->
             Bole.Tree.find store root_a k = Some v
             && Bole.Tree.find store root_b k = None
           | Bole.Diff.Modified (k, old_v, new_v) ->
             Bole.Tree.find store root_a k = Some old_v
             && Bole.Tree.find store root_b k = Some new_v
             && old_v <> new_v
         ) diff_entries
         (* And every actual difference should appear in the diff *)
         && begin
           let all_keys = List.sort_uniq String.compare
             (List.map fst sorted @ List.map fst mutations) in
           List.for_all (fun k ->
             let in_a = Bole.Tree.find store root_a k in
             let in_b = Bole.Tree.find store root_b k in
             match in_a, in_b with
             | None, None -> true
             | Some _, None ->
               List.exists (fun e -> diff_entry_key e = k) diff_entries
             | None, Some _ ->
               List.exists (fun e -> diff_entry_key e = k) diff_entries
             | Some va, Some vb ->
               if va = vb then
                 not (List.exists (fun e -> diff_entry_key e = k) diff_entries)
               else
                 List.exists (fun e -> diff_entry_key e = k) diff_entries
           ) all_keys
         end
       end)

let prop_diff_symmetry =
  QCheck2.Test.make ~name:"diff from/to is mirror of diff to/from"
    ~count:20
    QCheck2.Gen.(list_size (int_range 5 50)
      (pair
        (string_size ~gen:printable (int_range 1 10))
        (string_size ~gen:printable (int_range 1 10))))
    (fun pairs ->
       let sorted = List.sort_uniq (fun (k1, _) (k2, _) ->
         String.compare k1 k2) pairs in
       if List.length sorted < 3 then true
       else begin
         let n = List.length sorted in
         let half = n / 2 in
         let pairs_a = List.filteri (fun i _ -> i < half + half / 2) sorted in
         let pairs_b = List.filteri (fun i _ -> i >= half / 2) sorted in
         (* Modify some overlapping values *)
         let pairs_b = List.map (fun (k, v) ->
           if String.length k > 0 && Char.code k.[0] mod 3 = 0
           then (k, v ^ "-modified")
           else (k, v)
         ) pairs_b in
         let pairs_b = List.sort_uniq (fun (k1, _) (k2, _) ->
           String.compare k1 k2) pairs_b in
         let store = Bole.Store.create () in
         let root_a = Bole.Tree.build ~target_size:20 store (List.to_seq pairs_a) in
         let root_b = Bole.Tree.build ~target_size:20 store (List.to_seq pairs_b) in
         let forward = Bole.Diff.diff store ~from:root_a ~to_:root_b
           |> List.of_seq in
         let backward = Bole.Diff.diff store ~from:root_b ~to_:root_a
           |> List.of_seq in
         let mirror = function
           | Bole.Diff.Added (k, v) -> Bole.Diff.Removed (k, v)
           | Bole.Diff.Removed (k, v) -> Bole.Diff.Added (k, v)
           | Bole.Diff.Modified (k, o, n) -> Bole.Diff.Modified (k, n, o)
         in
         let mirrored = List.map mirror forward in
         List.length mirrored = List.length backward
         && List.for_all2 (fun a b ->
           diff_entry_key a = diff_entry_key b && a = b
         ) (List.sort (fun a b ->
              String.compare (diff_entry_key a) (diff_entry_key b)) mirrored)
            (List.sort (fun a b ->
              String.compare (diff_entry_key a) (diff_entry_key b)) backward)
       end)

let prop_apply_diff_round_trip =
  QCheck2.Test.make ~name:"applying diff to source produces target"
    ~count:20
    QCheck2.Gen.(pair
      (list_size (int_range 10 80)
        (pair
          (string_size ~gen:printable (int_range 1 10))
          (string_size ~gen:printable (int_range 1 10))))
      (list_size (int_range 1 15)
        (pair
          (string_size ~gen:printable (int_range 1 10))
          (string_size ~gen:printable (int_range 1 10)))))
    (fun (initial_pairs, mutations) ->
       let sorted = List.sort_uniq (fun (k1, _) (k2, _) ->
         String.compare k1 k2) initial_pairs in
       if List.length sorted < 5 then true
       else begin
         let store = Bole.Store.create () in
         let root_a = Bole.Tree.build ~target_size:20 store (List.to_seq sorted) in
         let root_b = List.fold_left (fun r (k, v) ->
           Bole.Tree.put ~target_size:20 store r k v
         ) root_a mutations in
         let diff_entries = Bole.Diff.diff store ~from:root_a ~to_:root_b
           |> List.of_seq in
         (* Apply diff to root_a *)
         let root_applied = List.fold_left (fun r e ->
           match e with
           | Bole.Diff.Added (k, v) ->
             Bole.Tree.put ~target_size:20 store r k v
           | Bole.Diff.Removed (k, _) ->
             Bole.Tree.delete ~target_size:20 store r k
           | Bole.Diff.Modified (k, _, new_v) ->
             Bole.Tree.put ~target_size:20 store r k new_v
         ) root_a diff_entries in
         (* root_applied should have same contents as root_b *)
         let entries_applied = Bole.Tree.range store root_applied |> List.of_seq in
         let entries_b = Bole.Tree.range store root_b |> List.of_seq in
         entries_applied = entries_b
       end)
```

Register them in the `tests` list:

```ocaml
let tests =
  [ "diff", [
      Alcotest.test_case "identical trees" `Quick test_identical_trees;
      Alcotest.test_case "empty vs non-empty" `Quick test_empty_vs_nonempty;
      Alcotest.test_case "single addition" `Quick test_single_addition;
      Alcotest.test_case "single deletion" `Quick test_single_deletion;
      Alcotest.test_case "single modification" `Quick test_single_modification;
      Alcotest.test_case "multiple changes" `Quick test_multiple_changes;
      Alcotest.test_case "multi-chunk diff" `Quick test_multi_chunk_diff;
      Alcotest.test_case "key ordering" `Quick test_key_ordering;
      QCheck_alcotest.to_alcotest prop_diff_completeness;
      QCheck_alcotest.to_alcotest prop_diff_symmetry;
      QCheck_alcotest.to_alcotest prop_apply_diff_round_trip;
    ]
  ]
```

**Step 2: Run tests to verify they pass**

Run: `guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && ALCOTEST_COLOR=never dune test' 2>&1`

Expected: All tests pass (74 tests — 63 existing + 11 new).

Note: If the symmetry property fails, the issue is likely in the key-ordering comparison. Make sure both forward and backward lists are sorted by key before comparing.

**Step 3: Commit**

```bash
git add test/test_diff.ml
git commit -m "test: add QCheck property tests for diff"
```
