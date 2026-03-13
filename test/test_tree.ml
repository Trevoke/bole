let test_empty_tree () =
  let store = Bole.Store.create () in
  let root = Bole.Tree.build store Seq.empty in
  (* Decoding the root should give an empty leaf *)
  let data = Bole.Store.get store root in
  let chunk = Bole.Chunk.decode data in
  Alcotest.(check bool) "empty tree is empty leaf"
    true (chunk = Bole.Chunk.Leaf [])

let tests =
  [ "tree", [
      Alcotest.test_case "empty tree" `Quick test_empty_tree;
    ]
  ]
