let with_tmp_dir f =
  let dir = Filename.temp_dir "bole-repo-test" "" in
  Fun.protect ~finally:(fun () ->
    let rec rm path =
      if Sys.is_directory path then begin
        Sys.readdir path |> Array.iter (fun name ->
          rm (Filename.concat path name));
        Sys.rmdir path
      end else
        Sys.remove path
    in
    rm dir
  ) (fun () -> f dir)

let test_init_creates_structure () =
  with_tmp_dir (fun dir ->
    Bole.Repo.init dir;
    Alcotest.(check bool) ".bole exists"
      true (Sys.file_exists (Filename.concat dir ".bole"));
    Alcotest.(check bool) "HEAD exists"
      true (Sys.file_exists (Filename.concat (Filename.concat dir ".bole") "HEAD"));
    Alcotest.(check bool) "objects exists"
      true (Sys.is_directory (Filename.concat (Filename.concat dir ".bole") "objects"));
    Alcotest.(check bool) "refs/heads exists"
      true (Sys.is_directory
        (Filename.concat (Filename.concat (Filename.concat dir ".bole") "refs") "heads")))

let test_round_trip () =
  with_tmp_dir (fun dir ->
    Bole.Repo.init dir;
    let db = Bole.Repo.load dir in
    let db = Bole.Db.put db ~table:"users" ~key:"alice" ~value:"admin" in
    let _h, db = Bole.Db.commit db ~message:"first" in
    Bole.Repo.save dir db;
    let db2 = Bole.Repo.load dir in
    Alcotest.(check (option string)) "persisted value"
      (Some "admin") (Bole.Db.find db2 ~table:"users" ~key:"alice"))

let test_branch_persistence () =
  with_tmp_dir (fun dir ->
    Bole.Repo.init dir;
    let db = Bole.Repo.load dir in
    let db = Bole.Db.put db ~table:"t" ~key:"k" ~value:"v" in
    let _, db = Bole.Db.commit db ~message:"init" in
    let db = Bole.Db.branch db ~name:"feature" in
    let db = Bole.Db.put db ~table:"t" ~key:"k2" ~value:"v2" in
    let _, db = Bole.Db.commit db ~message:"on feature" in
    Bole.Repo.save dir db;
    let db2 = Bole.Repo.load dir in
    Alcotest.(check string) "on feature branch" "feature"
      (Bole.Db.current_branch db2);
    Alcotest.(check (option string)) "feature has k2"
      (Some "v2") (Bole.Db.find db2 ~table:"t" ~key:"k2"))

let tests =
  [ "repo", [
      Alcotest.test_case "init creates structure" `Quick test_init_creates_structure;
      Alcotest.test_case "round-trip" `Quick test_round_trip;
      Alcotest.test_case "branch persistence" `Quick test_branch_persistence;
    ]
  ]
