let () =
  Alcotest.run "bole"
    (List.concat
       [ Test_hash.tests
       ; Test_store.tests
       ; Test_chunk.tests
       ; Test_chunker.tests
       ; Test_tree.tests
       ; Test_diff.tests
       ; Test_perf.tests
       ; Test_file_store.tests
       ; Test_db_state.tests
       ; Test_commit.tests
       ; Test_merge.tests
       ; Test_repo.tests
       ; Test_acceptance.tests
       ])
