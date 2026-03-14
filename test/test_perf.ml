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

let tests =
  [ "perf", [
      Alcotest.test_case "structural sharing" `Slow test_structural_sharing;
    ]
  ]
