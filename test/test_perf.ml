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
    (Some "val-000500") (Bole.Tree.find store root "key-000500")

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

let tests =
  [ "perf", [
      Alcotest.test_case "structural sharing" `Slow test_structural_sharing;
      Alcotest.test_case "diff efficiency" `Slow test_diff_efficiency;
    ]
  ]
