let () =
  Alcotest.run "bole"
    (List.concat
       [ Test_hash.tests
       ])
