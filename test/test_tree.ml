let test_empty_tree () =
  let store = Bole.Store.create () in
  let root = Bole.Tree.build store Seq.empty in
  (* Decoding the root should give an empty leaf *)
  let data = Bole.Store.get store root in
  let chunk = Bole.Chunk.decode data in
  Alcotest.(check bool) "empty tree is empty leaf"
    true (chunk = Bole.Chunk.Leaf [])

let test_single_entry () =
  let store = Bole.Store.create () in
  let root = Bole.Tree.build store
    (List.to_seq [("mykey", "myval")]) in
  let data = Bole.Store.get store root in
  let chunk = Bole.Chunk.decode data in
  match chunk with
  | Bole.Chunk.Leaf [e] ->
    Alcotest.(check string) "key" "mykey" e.key;
    Alcotest.(check string) "value" "myval" e.value
  | _ -> Alcotest.fail "expected single-entry leaf"

let test_small_tree_one_chunk () =
  let store = Bole.Store.create () in
  let pairs = List.init 10 (fun i ->
    (Printf.sprintf "key-%03d" i, Printf.sprintf "val-%03d" i)) in
  (* target_size:200 ensures no boundary is triggered for 10 entries *)
  let root = Bole.Tree.build ~target_size:200 store (List.to_seq pairs) in
  let data = Bole.Store.get store root in
  let chunk = Bole.Chunk.decode data in
  match chunk with
  | Bole.Chunk.Leaf entries ->
    Alcotest.(check int) "entry count" 10 (List.length entries);
    List.iteri (fun i (e : Bole.Chunk.leaf_entry) ->
      Alcotest.(check string) "key" (Printf.sprintf "key-%03d" i) e.key;
      Alcotest.(check string) "val" (Printf.sprintf "val-%03d" i) e.value
    ) entries
  | _ -> Alcotest.fail "expected leaf chunk"

let test_unsorted_raises () =
  let store = Bole.Store.create () in
  let pairs = List.to_seq [("b", "1"); ("a", "2")] in
  match Bole.Tree.build store pairs with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument for unsorted input"

let test_multi_chunk_tree () =
  let store = Bole.Store.create () in
  let pairs = List.init 200 (fun i ->
    (Printf.sprintf "key-%05d" i, Printf.sprintf "val-%05d" i)) in
  let root = Bole.Tree.build ~target_size:20 store (List.to_seq pairs) in
  (* Root should exist in store *)
  let data = Bole.Store.get store root in
  let root_chunk = Bole.Chunk.decode data in
  (* Root of a 200-entry tree with target 20 should be internal *)
  match root_chunk with
  | Bole.Chunk.Internal entries ->
    (* Internal node should have children *)
    Alcotest.(check bool) "has children" true (List.length entries > 0);
    (* Each child should exist in store *)
    List.iter (fun (e : Bole.Chunk.internal_entry) ->
      Alcotest.(check bool) "child in store"
        true (Bole.Store.mem store e.child)
    ) entries
  | Bole.Chunk.Leaf _ ->
    Alcotest.fail "expected internal root for 200 entries with target 20"

(** Walk a tree from root, collecting all leaf entries in order. *)
let walk_tree store root =
  let rec walk h =
    let data = Bole.Store.get store h in
    let chunk = Bole.Chunk.decode data in
    match chunk with
    | Bole.Chunk.Leaf entries ->
      List.map (fun (e : Bole.Chunk.leaf_entry) -> (e.key, e.value)) entries
    | Bole.Chunk.Internal entries ->
      List.concat_map (fun (e : Bole.Chunk.internal_entry) ->
        walk e.child) entries
  in
  walk root

let prop_round_trip =
  QCheck2.Test.make ~name:"round-trip: walk tree recovers all entries"
    ~count:20
    QCheck2.Gen.(list_size (int_range 0 300)
      (pair
        (string_size ~gen:printable (int_range 1 20))
        (string_size ~gen:printable (int_range 0 50))))
    (fun pairs ->
       let sorted = List.sort_uniq (fun (k1, _) (k2, _) ->
         String.compare k1 k2) pairs in
       let store = Bole.Store.create () in
       let root = Bole.Tree.build ~target_size:20 store
         (List.to_seq sorted) in
       let recovered = walk_tree store root in
       recovered = sorted)

let prop_history_independence =
  QCheck2.Test.make ~name:"history independence: same data → same root"
    ~count:20
    QCheck2.Gen.(list_size (int_range 1 200)
      (pair
        (string_size ~gen:printable (int_range 1 20))
        (string_size ~gen:printable (int_range 0 50))))
    (fun pairs ->
       let sorted = List.sort_uniq (fun (k1, _) (k2, _) ->
         String.compare k1 k2) pairs in
       let store1 = Bole.Store.create () in
       let root1 = Bole.Tree.build ~target_size:20 store1
         (List.to_seq sorted) in
       let store2 = Bole.Store.create () in
       let root2 = Bole.Tree.build ~target_size:20 store2
         (List.to_seq sorted) in
       Bole.Hash.equal root1 root2)

let prop_level_independence =
  QCheck2.Test.make ~name:"level independence: different target_size, same leaves"
    ~count:10
    QCheck2.Gen.(list_size (int_range 10 200)
      (pair
        (string_size ~gen:printable (int_range 1 20))
        (string_size ~gen:printable (int_range 0 50))))
    (fun pairs ->
       let sorted = List.sort_uniq (fun (k1, _) (k2, _) ->
         String.compare k1 k2) pairs in
       if List.length sorted < 2 then true
       else begin
         let store1 = Bole.Store.create () in
         let root1 = Bole.Tree.build ~target_size:15 store1
           (List.to_seq sorted) in
         let store2 = Bole.Store.create () in
         let root2 = Bole.Tree.build ~target_size:30 store2
           (List.to_seq sorted) in
         let leaves1 = walk_tree store1 root1 in
         let leaves2 = walk_tree store2 root2 in
         leaves1 = leaves2
       end)

let test_duplicate_keys_allowed () =
  let store = Bole.Store.create () in
  let pairs = List.to_seq [("a", "1"); ("a", "2"); ("b", "3")] in
  let root = Bole.Tree.build store pairs in
  let recovered = walk_tree store root in
  Alcotest.(check int) "all entries present" 3 (List.length recovered);
  Alcotest.(check (list (pair string string))) "entries in order"
    [("a", "1"); ("a", "2"); ("b", "3")] recovered

let test_find_existing () =
  let store = Bole.Store.create () in
  let pairs = List.init 10 (fun i ->
    (Printf.sprintf "key-%03d" i, Printf.sprintf "val-%03d" i)) in
  let root = Bole.Tree.build ~target_size:200 store (List.to_seq pairs) in
  Alcotest.(check (option string)) "find key-005"
    (Some "val-005") (Bole.Tree.find store root "key-005")

let test_find_missing () =
  let store = Bole.Store.create () in
  let pairs = List.init 10 (fun i ->
    (Printf.sprintf "key-%03d" i, Printf.sprintf "val-%03d" i)) in
  let root = Bole.Tree.build ~target_size:200 store (List.to_seq pairs) in
  Alcotest.(check (option string)) "find nonexistent"
    None (Bole.Tree.find store root "zzz")

let test_find_empty_tree () =
  let store = Bole.Store.create () in
  let root = Bole.Tree.build store Seq.empty in
  Alcotest.(check (option string)) "find in empty"
    None (Bole.Tree.find store root "anything")

let test_find_multi_chunk () =
  let store = Bole.Store.create () in
  let pairs = List.init 200 (fun i ->
    (Printf.sprintf "key-%05d" i, Printf.sprintf "val-%05d" i)) in
  let root = Bole.Tree.build ~target_size:20 store (List.to_seq pairs) in
  Alcotest.(check (option string)) "find first"
    (Some "val-00000") (Bole.Tree.find store root "key-00000");
  Alcotest.(check (option string)) "find middle"
    (Some "val-00100") (Bole.Tree.find store root "key-00100");
  Alcotest.(check (option string)) "find last"
    (Some "val-00199") (Bole.Tree.find store root "key-00199");
  Alcotest.(check (option string)) "find missing in multi"
    None (Bole.Tree.find store root "key-00200")

let prop_find_round_trip =
  QCheck2.Test.make ~name:"find returns correct value for all inserted keys"
    ~count:20
    QCheck2.Gen.(list_size (int_range 1 300)
      (pair
        (string_size ~gen:printable (int_range 1 20))
        (string_size ~gen:printable (int_range 0 50))))
    (fun pairs ->
       let sorted = List.sort_uniq (fun (k1, _) (k2, _) ->
         String.compare k1 k2) pairs in
       let store = Bole.Store.create () in
       let root = Bole.Tree.build ~target_size:20 store
         (List.to_seq sorted) in
       List.for_all (fun (k, v) ->
         Bole.Tree.find store root k = Some v
       ) sorted
       &&
       Bole.Tree.find store root "\xff\xff\xff" = None)

let test_range_full_scan () =
  let store = Bole.Store.create () in
  let pairs = List.init 200 (fun i ->
    (Printf.sprintf "key-%05d" i, Printf.sprintf "val-%05d" i)) in
  let root = Bole.Tree.build ~target_size:20 store (List.to_seq pairs) in
  let result = Bole.Tree.range store root |> List.of_seq in
  Alcotest.(check (list (pair string string))) "full scan"
    pairs result

let test_range_empty_tree () =
  let store = Bole.Store.create () in
  let root = Bole.Tree.build store Seq.empty in
  let result = Bole.Tree.range store root |> List.of_seq in
  Alcotest.(check (list (pair string string))) "empty range"
    [] result

let test_range_start_key () =
  let store = Bole.Store.create () in
  let pairs = List.init 200 (fun i ->
    (Printf.sprintf "key-%05d" i, Printf.sprintf "val-%05d" i)) in
  let root = Bole.Tree.build ~target_size:20 store (List.to_seq pairs) in
  let result = Bole.Tree.range ~start_key:"key-00100" store root
    |> List.of_seq in
  let expected = List.filteri (fun i _ -> i >= 100) pairs in
  Alcotest.(check int) "start_key count" 100 (List.length result);
  Alcotest.(check (list (pair string string))) "start_key entries"
    expected result

let test_range_end_key () =
  let store = Bole.Store.create () in
  let pairs = List.init 200 (fun i ->
    (Printf.sprintf "key-%05d" i, Printf.sprintf "val-%05d" i)) in
  let root = Bole.Tree.build ~target_size:20 store (List.to_seq pairs) in
  let result = Bole.Tree.range ~end_key:"key-00050" store root
    |> List.of_seq in
  let expected = List.filteri (fun i _ -> i < 50) pairs in
  Alcotest.(check int) "end_key count" 50 (List.length result);
  Alcotest.(check (list (pair string string))) "end_key entries"
    expected result

let test_range_both_bounds () =
  let store = Bole.Store.create () in
  let pairs = List.init 200 (fun i ->
    (Printf.sprintf "key-%05d" i, Printf.sprintf "val-%05d" i)) in
  let root = Bole.Tree.build ~target_size:20 store (List.to_seq pairs) in
  let result = Bole.Tree.range ~start_key:"key-00050"
    ~end_key:"key-00100" store root |> List.of_seq in
  let expected = List.filteri (fun i _ -> i >= 50 && i < 100) pairs in
  Alcotest.(check int) "both bounds count" 50 (List.length result);
  Alcotest.(check (list (pair string string))) "both bounds entries"
    expected result

let test_range_empty_result () =
  let store = Bole.Store.create () in
  let pairs = List.init 10 (fun i ->
    (Printf.sprintf "key-%03d" i, Printf.sprintf "val-%03d" i)) in
  let root = Bole.Tree.build ~target_size:200 store (List.to_seq pairs) in
  let result = Bole.Tree.range ~start_key:"zzz" store root
    |> List.of_seq in
  Alcotest.(check (list (pair string string))) "empty range" [] result

let prop_range_full_scan =
  QCheck2.Test.make ~name:"range full scan equals input"
    ~count:20
    QCheck2.Gen.(list_size (int_range 0 300)
      (pair
        (string_size ~gen:printable (int_range 1 20))
        (string_size ~gen:printable (int_range 0 50))))
    (fun pairs ->
       let sorted = List.sort_uniq (fun (k1, _) (k2, _) ->
         String.compare k1 k2) pairs in
       let store = Bole.Store.create () in
       let root = Bole.Tree.build ~target_size:20 store
         (List.to_seq sorted) in
       let result = Bole.Tree.range store root |> List.of_seq in
       result = sorted)

let prop_range_bounds_filter =
  QCheck2.Test.make ~name:"range with bounds equals list filter"
    ~count:20
    QCheck2.Gen.(list_size (int_range 10 200)
      (pair
        (string_size ~gen:printable (int_range 1 20))
        (string_size ~gen:printable (int_range 0 50))))
    (fun pairs ->
       let sorted = List.sort_uniq (fun (k1, _) (k2, _) ->
         String.compare k1 k2) pairs in
       if List.length sorted < 3 then true
       else begin
         let store = Bole.Store.create () in
         let root = Bole.Tree.build ~target_size:20 store
           (List.to_seq sorted) in
         let n = List.length sorted in
         let start_idx = n / 4 in
         let end_idx = 3 * n / 4 in
         let start_key = fst (List.nth sorted start_idx) in
         let end_key = fst (List.nth sorted end_idx) in
         let result = Bole.Tree.range ~start_key ~end_key store root
           |> List.of_seq in
         let expected = List.filter (fun (k, _) ->
           k >= start_key && k < end_key) sorted in
         result = expected
       end)

let test_put_empty_tree () =
  let store = Bole.Store.create () in
  let root = Bole.Tree.build store Seq.empty in
  let root' = Bole.Tree.put store root "hello" "world" in
  Alcotest.(check (option string)) "find after put"
    (Some "world") (Bole.Tree.find store root' "hello")

let test_put_new_key () =
  let store = Bole.Store.create () in
  let pairs = List.init 10 (fun i ->
    (Printf.sprintf "key-%03d" i, Printf.sprintf "val-%03d" i)) in
  let root = Bole.Tree.build ~target_size:200 store (List.to_seq pairs) in
  let root' = Bole.Tree.put store root "key-005a" "new-val" in
  Alcotest.(check (option string)) "find new key"
    (Some "new-val") (Bole.Tree.find store root' "key-005a");
  Alcotest.(check (option string)) "old key intact"
    (Some "val-005") (Bole.Tree.find store root' "key-005");
  Alcotest.(check (option string)) "old tree unchanged"
    None (Bole.Tree.find store root "key-005a")

let test_put_update_existing () =
  let store = Bole.Store.create () in
  let pairs = List.init 10 (fun i ->
    (Printf.sprintf "key-%03d" i, Printf.sprintf "val-%03d" i)) in
  let root = Bole.Tree.build ~target_size:200 store (List.to_seq pairs) in
  let root' = Bole.Tree.put store root "key-005" "updated" in
  Alcotest.(check (option string)) "value updated"
    (Some "updated") (Bole.Tree.find store root' "key-005");
  Alcotest.(check (option string)) "old tree unchanged"
    (Some "val-005") (Bole.Tree.find store root "key-005")

let test_put_same_value_noop () =
  let store = Bole.Store.create () in
  let pairs = List.init 10 (fun i ->
    (Printf.sprintf "key-%03d" i, Printf.sprintf "val-%03d" i)) in
  let root = Bole.Tree.build ~target_size:200 store (List.to_seq pairs) in
  let root' = Bole.Tree.put store root "key-005" "val-005" in
  Alcotest.(check bool) "same root hash"
    true (Bole.Hash.equal root root')

let test_put_multi_chunk () =
  let store = Bole.Store.create () in
  let pairs = List.init 200 (fun i ->
    (Printf.sprintf "key-%05d" i, Printf.sprintf "val-%05d" i)) in
  let root = Bole.Tree.build ~target_size:20 store (List.to_seq pairs) in
  let root' = Bole.Tree.put store root "key-00000a" "inserted-begin" in
  Alcotest.(check (option string)) "inserted at begin"
    (Some "inserted-begin") (Bole.Tree.find store root' "key-00000a");
  let root'' = Bole.Tree.put store root' "key-00100a" "inserted-mid" in
  Alcotest.(check (option string)) "inserted at mid"
    (Some "inserted-mid") (Bole.Tree.find store root'' "key-00100a");
  let root''' = Bole.Tree.put store root'' "key-00050" "updated" in
  Alcotest.(check (option string)) "updated value"
    (Some "updated") (Bole.Tree.find store root''' "key-00050");
  Alcotest.(check (option string)) "original intact"
    (Some "val-00199") (Bole.Tree.find store root''' "key-00199")

let tests =
  [ "tree", [
      Alcotest.test_case "empty tree" `Quick test_empty_tree;
      Alcotest.test_case "single entry" `Quick test_single_entry;
      Alcotest.test_case "small tree (one chunk)" `Quick test_small_tree_one_chunk;
      Alcotest.test_case "unsorted input raises" `Quick test_unsorted_raises;
      Alcotest.test_case "multi-chunk tree" `Quick test_multi_chunk_tree;
      Alcotest.test_case "duplicate keys allowed" `Quick test_duplicate_keys_allowed;
      QCheck_alcotest.to_alcotest prop_round_trip;
      QCheck_alcotest.to_alcotest prop_history_independence;
      QCheck_alcotest.to_alcotest prop_level_independence;
      Alcotest.test_case "find existing" `Quick test_find_existing;
      Alcotest.test_case "find missing" `Quick test_find_missing;
      Alcotest.test_case "find in empty tree" `Quick test_find_empty_tree;
      Alcotest.test_case "find in multi-chunk tree" `Quick test_find_multi_chunk;
      QCheck_alcotest.to_alcotest prop_find_round_trip;
      Alcotest.test_case "range full scan" `Quick test_range_full_scan;
      Alcotest.test_case "range on empty tree" `Quick test_range_empty_tree;
      Alcotest.test_case "range with start_key" `Quick test_range_start_key;
      Alcotest.test_case "range with end_key" `Quick test_range_end_key;
      Alcotest.test_case "range with both bounds" `Quick test_range_both_bounds;
      Alcotest.test_case "range empty result" `Quick test_range_empty_result;
      QCheck_alcotest.to_alcotest prop_range_full_scan;
      QCheck_alcotest.to_alcotest prop_range_bounds_filter;
      Alcotest.test_case "put into empty tree" `Quick test_put_empty_tree;
      Alcotest.test_case "put new key" `Quick test_put_new_key;
      Alcotest.test_case "put update existing" `Quick test_put_update_existing;
      Alcotest.test_case "put same value noop" `Quick test_put_same_value_noop;
      Alcotest.test_case "put in multi-chunk tree" `Quick test_put_multi_chunk;
    ]
  ]
