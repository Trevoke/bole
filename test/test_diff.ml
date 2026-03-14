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

let tests =
  [ "diff", [
      Alcotest.test_case "identical trees" `Quick test_identical_trees;
      Alcotest.test_case "empty vs non-empty" `Quick test_empty_vs_nonempty;
      Alcotest.test_case "single addition" `Quick test_single_addition;
      Alcotest.test_case "single deletion" `Quick test_single_deletion;
      Alcotest.test_case "single modification" `Quick test_single_modification;
    ]
  ]
