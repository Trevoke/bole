let () =
  Alcotest.run "bole"
    (List.concat
       [ Test_hash.tests
       ; Test_store.tests
       ; Test_chunk.tests
       ; Test_chunker.tests
       ; Test_tree.tests
       ])
