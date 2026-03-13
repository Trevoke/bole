let () =
  Alcotest.run "bole"
    (List.concat
       [ Test_hash.tests
       ; Test_store.tests
       ])
