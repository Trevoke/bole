# Three-Way Merge Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement the `Merge` module (diff stream reconciliation) and `Db.merge` (ancestor finding + per-table dispatch), making acceptance tests 5 and 6 pass.

**Architecture:** New `Merge` module with `three_way` function that merge-joins two `Diff.entry Seq.t` streams, producing changes to apply plus conflicts. `Db.merge` orchestrates: find common ancestor via BFS, load three database states, dispatch per-table reconciliation, apply non-conflicting changes via Tree.put/delete.

**Tech Stack:** OCaml, Alcotest, dune.

**Environment:** Run all commands inside `guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && ...'`. Use `ALCOTEST_COLOR=never` for readable test output.

**Design doc:** `docs/plans/2026-03-14-merge-design.md`

---

### Task 7a: Merge module skeleton with basic unit tests

**Files:**
- Create: `lib/merge.mli`
- Create: `lib/merge.ml`
- Create: `test/test_merge.ml`
- Modify: `lib/bole.ml` (add `module Merge = Merge`)
- Modify: `test/test_main.ml` (add `Test_merge.tests`)

**Step 1: Create the interface**

Create `lib/merge.mli`:

```ocaml
(** Three-way merge of two diff streams.

    Given two diff streams (ancestor→ours and ancestor→theirs),
    merge-joins them by key to produce non-conflicting changes
    and a list of conflicts.

    Changes are relative to the ours tree — only theirs-side
    non-conflicting changes appear in the changes list. *)

type change =
  | Put of string * string
  | Delete of string

type conflict = {
  key : string;
  base : string option;
  ours : string option;
  theirs : string option;
}

type result = {
  changes : change list;
  conflicts : conflict list;
}

val three_way : ours:Diff.entry Seq.t -> theirs:Diff.entry Seq.t -> result
```

**Step 2: Create the implementation**

Create `lib/merge.ml`:

```ocaml
type change =
  | Put of string * string
  | Delete of string

type conflict = {
  key : string;
  base : string option;
  ours : string option;
  theirs : string option;
}

type result = {
  changes : change list;
  conflicts : conflict list;
}

let diff_entry_key = function
  | Diff.Added (k, _) -> k
  | Diff.Removed (k, _) -> k
  | Diff.Modified (k, _, _) -> k

let diff_entry_equal a b =
  match a, b with
  | Diff.Added (k1, v1), Diff.Added (k2, v2) -> k1 = k2 && v1 = v2
  | Diff.Removed (k1, v1), Diff.Removed (k2, v2) -> k1 = k2 && v1 = v2
  | Diff.Modified (k1, o1, n1), Diff.Modified (k2, o2, n2) ->
    k1 = k2 && o1 = o2 && n1 = n2
  | _ -> false

let change_of_theirs_entry = function
  | Diff.Added (k, v) -> Put (k, v)
  | Diff.Removed (_, v) -> Delete v
  | Diff.Modified (k, _, new_v) -> Put (k, new_v)

let conflict_of_entries ~ours_entry ~theirs_entry =
  let key = diff_entry_key ours_entry in
  let base =
    match ours_entry with
    | Diff.Modified (_, old_v, _) -> Some old_v
    | Diff.Removed (_, old_v) -> Some old_v
    | Diff.Added _ -> None
  in
  let ours =
    match ours_entry with
    | Diff.Added (_, v) | Diff.Modified (_, _, v) -> Some v
    | Diff.Removed _ -> None
  in
  let theirs =
    match theirs_entry with
    | Diff.Added (_, v) | Diff.Modified (_, _, v) -> Some v
    | Diff.Removed _ -> None
  in
  { key; base; ours; theirs }

let three_way ~ours ~theirs =
  let rec go ours theirs changes_acc conflicts_acc =
    match ours (), theirs () with
    | Seq.Nil, Seq.Nil ->
      { changes = List.rev changes_acc; conflicts = List.rev conflicts_acc }
    | Seq.Nil, Seq.Cons (t_entry, theirs_rest) ->
      (* Only theirs — apply *)
      let change = change_of_theirs_entry t_entry in
      go (fun () -> Seq.Nil) theirs_rest (change :: changes_acc) conflicts_acc
    | Seq.Cons (_, ours_rest), Seq.Nil ->
      (* Only ours — already applied, skip *)
      go ours_rest (fun () -> Seq.Nil) changes_acc conflicts_acc
    | Seq.Cons (o_entry, ours_rest), Seq.Cons (t_entry, theirs_rest) ->
      let o_key = diff_entry_key o_entry in
      let t_key = diff_entry_key t_entry in
      let cmp = String.compare o_key t_key in
      if cmp < 0 then
        (* Only ours — skip *)
        go ours_rest theirs changes_acc conflicts_acc
      else if cmp > 0 then
        (* Only theirs — apply *)
        let change = change_of_theirs_entry t_entry in
        go ours theirs_rest (change :: changes_acc) conflicts_acc
      else if diff_entry_equal o_entry t_entry then
        (* Same change — skip *)
        go ours_rest theirs_rest changes_acc conflicts_acc
      else
        (* Different change — conflict *)
        let conflict = conflict_of_entries ~ours_entry:o_entry ~theirs_entry:t_entry in
        go ours_rest theirs_rest changes_acc (conflict :: conflicts_acc)
  in
  go ours theirs [] []
```

Note: `change_of_theirs_entry` for `Removed` should use `Delete` with the key, not the value. Fix:

```ocaml
let change_of_theirs_entry = function
  | Diff.Added (k, v) -> Put (k, v)
  | Diff.Removed (k, _) -> Delete k
  | Diff.Modified (k, _, new_v) -> Put (k, new_v)
```

**Step 3: Write unit tests**

Create `test/test_merge.ml`:

```ocaml
let test_both_empty () =
  let result = Bole.Merge.three_way ~ours:Seq.empty ~theirs:Seq.empty in
  Alcotest.(check int) "no changes" 0 (List.length result.Bole.Merge.changes);
  Alcotest.(check int) "no conflicts" 0 (List.length result.Bole.Merge.conflicts)

let test_only_theirs () =
  let theirs = List.to_seq [
    Bole.Diff.Added ("b", "val-b");
    Bole.Diff.Modified ("c", "old-c", "new-c");
    Bole.Diff.Removed ("d", "val-d");
  ] in
  let result = Bole.Merge.three_way ~ours:Seq.empty ~theirs in
  Alcotest.(check int) "3 changes" 3 (List.length result.Bole.Merge.changes);
  Alcotest.(check int) "no conflicts" 0 (List.length result.Bole.Merge.conflicts);
  (match result.Bole.Merge.changes with
   | [Bole.Merge.Put ("b", "val-b");
      Bole.Merge.Put ("c", "new-c");
      Bole.Merge.Delete "d"] -> ()
   | _ -> Alcotest.fail "unexpected changes")

let test_only_ours () =
  let ours = List.to_seq [
    Bole.Diff.Added ("a", "val-a");
    Bole.Diff.Modified ("b", "old", "new");
  ] in
  let result = Bole.Merge.three_way ~ours ~theirs:Seq.empty in
  Alcotest.(check int) "no changes" 0 (List.length result.Bole.Merge.changes);
  Alcotest.(check int) "no conflicts" 0 (List.length result.Bole.Merge.conflicts)

let test_same_change () =
  let entry = Bole.Diff.Modified ("a", "old", "new") in
  let ours = List.to_seq [entry] in
  let theirs = List.to_seq [entry] in
  let result = Bole.Merge.three_way ~ours ~theirs in
  Alcotest.(check int) "no changes" 0 (List.length result.Bole.Merge.changes);
  Alcotest.(check int) "no conflicts" 0 (List.length result.Bole.Merge.conflicts)

let test_different_modify () =
  let ours = List.to_seq [Bole.Diff.Modified ("a", "base", "ours-val")] in
  let theirs = List.to_seq [Bole.Diff.Modified ("a", "base", "theirs-val")] in
  let result = Bole.Merge.three_way ~ours ~theirs in
  Alcotest.(check int) "no changes" 0 (List.length result.Bole.Merge.changes);
  Alcotest.(check int) "one conflict" 1 (List.length result.Bole.Merge.conflicts);
  let c = List.hd result.Bole.Merge.conflicts in
  Alcotest.(check string) "key" "a" c.Bole.Merge.key;
  Alcotest.(check (option string)) "base" (Some "base") c.Bole.Merge.base;
  Alcotest.(check (option string)) "ours" (Some "ours-val") c.Bole.Merge.ours;
  Alcotest.(check (option string)) "theirs" (Some "theirs-val") c.Bole.Merge.theirs

let test_both_added_different () =
  let ours = List.to_seq [Bole.Diff.Added ("a", "v1")] in
  let theirs = List.to_seq [Bole.Diff.Added ("a", "v2")] in
  let result = Bole.Merge.three_way ~ours ~theirs in
  Alcotest.(check int) "no changes" 0 (List.length result.Bole.Merge.changes);
  Alcotest.(check int) "one conflict" 1 (List.length result.Bole.Merge.conflicts);
  let c = List.hd result.Bole.Merge.conflicts in
  Alcotest.(check (option string)) "base" None c.Bole.Merge.base;
  Alcotest.(check (option string)) "ours" (Some "v1") c.Bole.Merge.ours;
  Alcotest.(check (option string)) "theirs" (Some "v2") c.Bole.Merge.theirs

let test_mixed () =
  let ours = List.to_seq [
    Bole.Diff.Modified ("a", "old-a", "ours-a");
    Bole.Diff.Added ("c", "ours-c");
    Bole.Diff.Modified ("d", "old-d", "new-d");
  ] in
  let theirs = List.to_seq [
    Bole.Diff.Added ("b", "theirs-b");
    Bole.Diff.Added ("c", "theirs-c");
    Bole.Diff.Modified ("d", "old-d", "new-d");
  ] in
  let result = Bole.Merge.three_way ~ours ~theirs in
  (* b: only theirs → change Put *)
  (* a: only ours → no change *)
  (* c: both added different → conflict *)
  (* d: same change → no change *)
  Alcotest.(check int) "1 change" 1 (List.length result.Bole.Merge.changes);
  (match result.Bole.Merge.changes with
   | [Bole.Merge.Put ("b", "theirs-b")] -> ()
   | _ -> Alcotest.fail "expected Put(b)");
  Alcotest.(check int) "1 conflict" 1 (List.length result.Bole.Merge.conflicts);
  Alcotest.(check string) "conflict key" "c"
    (List.hd result.Bole.Merge.conflicts).Bole.Merge.key

let tests =
  [ "merge", [
      Alcotest.test_case "both empty" `Quick test_both_empty;
      Alcotest.test_case "only theirs" `Quick test_only_theirs;
      Alcotest.test_case "only ours" `Quick test_only_ours;
      Alcotest.test_case "same change" `Quick test_same_change;
      Alcotest.test_case "different modify" `Quick test_different_modify;
      Alcotest.test_case "both added different" `Quick test_both_added_different;
      Alcotest.test_case "mixed" `Quick test_mixed;
    ]
  ]
```

**Step 4: Wire up**

Add `module Merge = Merge` to `lib/bole.ml` (after `module Commit = Commit`, before `module Db = Db`).

Add `Test_merge.tests` to `test/test_main.ml` (after `Test_commit.tests`, before `Test_acceptance.tests`).

**Step 5: Run tests and commit**

Run: `guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && ALCOTEST_COLOR=never dune test'`

Expected: 97 tests total (90 existing + 7 new merge tests). The 7 new merge tests pass. Acceptance tests 5/6 still fail (Db.merge not wired up yet).

```bash
git add lib/merge.mli lib/merge.ml lib/bole.ml test/test_merge.ml test/test_main.ml
git commit -m "feat: add Merge module with three-way diff stream reconciliation"
```

---

### Task 7b: Db.merge — find_ancestor and per-table dispatch

**Files:**
- Modify: `lib/db.ml`

**Step 1: Implement `find_ancestor`**

Add a private helper in `lib/db.ml` (before the `merge` function):

```ocaml
let load_tables db commit_hash =
  let commit_data = Store.get db.store commit_hash in
  let commit_obj = Commit.decode commit_data in
  let state_data = Store.get db.store commit_obj.state in
  let entries = Db_state.decode state_data in
  List.fold_left (fun acc (e : Db_state.table_entry) ->
    StringMap.add e.name e.root acc
  ) StringMap.empty entries

let find_ancestor db h1 h2 =
  (* BFS from both sides, first shared hash is the ancestor *)
  let module HashSet = Set.Make(struct
    type t = Hash.t
    let compare = Hash.compare
  end) in
  let seen1 = ref (HashSet.singleton h1) in
  let seen2 = ref (HashSet.singleton h2) in
  let queue1 = Queue.create () in
  let queue2 = Queue.create () in
  Queue.push h1 queue1;
  Queue.push h2 queue2;
  (* Check if any hash is in both sets *)
  if Hash.equal h1 h2 then h1
  else begin
    let found = ref None in
    while !found = None && (not (Queue.is_empty queue1) || not (Queue.is_empty queue2)) do
      (* Expand queue1 *)
      if not (Queue.is_empty queue1) then begin
        let h = Queue.pop queue1 in
        let ps = parents db h in
        List.iter (fun p ->
          if HashSet.mem p !seen2 then found := Some p
          else if not (HashSet.mem p !seen1) then begin
            seen1 := HashSet.add p !seen1;
            Queue.push p queue1
          end
        ) ps
      end;
      (* Expand queue2 *)
      if !found = None && not (Queue.is_empty queue2) then begin
        let h = Queue.pop queue2 in
        let ps = parents db h in
        List.iter (fun p ->
          if HashSet.mem p !seen1 then found := Some p
          else if not (HashSet.mem p !seen2) then begin
            seen2 := HashSet.add p !seen2;
            Queue.push p queue2
          end
        ) ps
      end
    done;
    match !found with
    | Some h -> h
    | None -> raise Not_found
  end
```

**Step 2: Implement `merge`**

Replace the `merge` stub:

```ocaml
let merge db ~ours ~theirs =
  let ours_hash = Hashtbl.find db.branches ours in
  let theirs_hash = Hashtbl.find db.branches theirs in
  let ancestor_hash = find_ancestor db ours_hash theirs_hash in
  let ancestor_tables = load_tables db ancestor_hash in
  let ours_tables = load_tables db ours_hash in
  let theirs_tables = load_tables db theirs_hash in
  (* Collect union of all table names *)
  let all_names = StringMap.empty
    |> StringMap.union (fun _ a _ -> Some a) ancestor_tables
    |> StringMap.union (fun _ a _ -> Some a) ours_tables
    |> StringMap.union (fun _ a _ -> Some a) theirs_tables
  in
  let empty_root = empty_tree_root db.store in
  let merged_tables = ref StringMap.empty in
  let all_conflicts = ref [] in
  StringMap.iter (fun name _ ->
    let a_root = match StringMap.find_opt name ancestor_tables with
      | Some r -> r | None -> empty_root in
    let o_root = match StringMap.find_opt name ours_tables with
      | Some r -> r | None -> empty_root in
    let t_root = match StringMap.find_opt name theirs_tables with
      | Some r -> r | None -> empty_root in
    if Hash.equal o_root t_root then
      (* Both same — keep ours *)
      merged_tables := StringMap.add name o_root !merged_tables
    else if Hash.equal o_root a_root then
      (* Only theirs changed — take theirs *)
      merged_tables := StringMap.add name t_root !merged_tables
    else if Hash.equal t_root a_root then
      (* Only ours changed — keep ours *)
      merged_tables := StringMap.add name o_root !merged_tables
    else begin
      (* Both differ from ancestor — three-way merge *)
      let ours_diff = Diff.diff db.store ~from:a_root ~to_:o_root in
      let theirs_diff = Diff.diff db.store ~from:a_root ~to_:t_root in
      let result = Merge.three_way ~ours:ours_diff ~theirs:theirs_diff in
      (* Apply changes to ours tree *)
      let final_root = List.fold_left (fun root change ->
        match change with
        | Merge.Put (k, v) -> Tree.put db.store root k v
        | Merge.Delete k -> Tree.delete db.store root k
      ) o_root result.Merge.changes in
      merged_tables := StringMap.add name final_root !merged_tables;
      (* Collect conflicts with table name *)
      List.iter (fun (c : Merge.conflict) ->
        all_conflicts := {
          table = name;
          key = c.key;
          base = c.base;
          ours = c.ours;
          theirs = c.theirs;
        } :: !all_conflicts
      ) result.Merge.conflicts
    end
  ) all_names;
  { db = { db with tables = !merged_tables };
    conflicts = List.rev !all_conflicts }
```

**Step 3: Run tests**

Run: `guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && ALCOTEST_COLOR=never dune test' 2>&1`

Expected: All 97 tests pass, including acceptance tests 5 ("clean merge") and 6 ("conflicting merge").

**Step 4: Commit**

```bash
git add lib/db.ml
git commit -m "feat: add Db.merge with ancestor finding and three-way reconciliation"
```
