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

let tests =
  [ "tree", [
      Alcotest.test_case "empty tree" `Quick test_empty_tree;
      Alcotest.test_case "single entry" `Quick test_single_entry;
      Alcotest.test_case "small tree (one chunk)" `Quick test_small_tree_one_chunk;
      Alcotest.test_case "unsorted input raises" `Quick test_unsorted_raises;
    ]
  ]
