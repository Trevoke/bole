let test_identical_trees () =
  let store = Bole.Store.create () in
  let pairs = List.init 10 (fun i ->
    (Printf.sprintf "key-%03d" i, Printf.sprintf "val-%03d" i)) in
  let root = Bole.Tree.build store (List.to_seq pairs) in
  let entries = Bole.Diff.diff store ~from:root ~to_:root |> List.of_seq in
  Alcotest.(check int) "no differences" 0 (List.length entries)

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
