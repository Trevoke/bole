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
    ]
  ]
