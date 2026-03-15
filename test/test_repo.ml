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

let simple_schema = Bole.Schema.create
  ~columns:["name", Bole.Schema.Str; "value", Bole.Schema.Str]

let tuple_value = Alcotest.testable
  (fun fmt v -> match v with
    | Bole.Tuple.String s -> Format.fprintf fmt "String %S" s
    | Bole.Tuple.Int64 n -> Format.fprintf fmt "Int64 %Ld" n
    | Bole.Tuple.Uuid u -> Format.fprintf fmt "Uuid %s" (Bole.Uuid.to_hex u)
    | Bole.Tuple.Bool b -> Format.fprintf fmt "Bool %b" b
    | Bole.Tuple.Float f -> Format.fprintf fmt "Float %g" f
    | Bole.Tuple.Timestamp ts -> Format.fprintf fmt "Timestamp %Ld" ts
    | Bole.Tuple.Blob b -> Format.fprintf fmt "Blob %S" b)
  (=)

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
    let db = Bole.Db.create_table db ~table:"users" ~schema:simple_schema in
    let alice_id, db = Bole.Db.put_row db ~table:"users" ~row:["name", Bole.Tuple.String "alice"; "value", Bole.Tuple.String "admin"] in
    let _h, db = Bole.Db.commit db ~message:"first" in
    Bole.Repo.save dir db;
    let db2 = Bole.Repo.load dir in
    match Bole.Db.get_row db2 ~table:"users" ~id:alice_id with
    | Some row ->
      Alcotest.(check tuple_value) "persisted value"
        (Bole.Tuple.String "admin") (List.assoc "value" row)
    | None -> Alcotest.fail "alice not found after reload")

let test_branch_persistence () =
  with_tmp_dir (fun dir ->
    Bole.Repo.init dir;
    let db = Bole.Repo.load dir in
    let db = Bole.Db.create_table db ~table:"t" ~schema:simple_schema in
    let _k_id, db = Bole.Db.put_row db ~table:"t" ~row:["name", Bole.Tuple.String "k"; "value", Bole.Tuple.String "v"] in
    let _, db = Bole.Db.commit db ~message:"init" in
    let db = Bole.Db.branch db ~name:"feature" in
    let k2_id, db = Bole.Db.put_row db ~table:"t" ~row:["name", Bole.Tuple.String "k2"; "value", Bole.Tuple.String "v2"] in
    let _, db = Bole.Db.commit db ~message:"on feature" in
    Bole.Repo.save dir db;
    let db2 = Bole.Repo.load dir in
    Alcotest.(check string) "on feature branch" "feature"
      (Bole.Db.current_branch db2);
    match Bole.Db.get_row db2 ~table:"t" ~id:k2_id with
    | Some row ->
      Alcotest.(check tuple_value) "feature has k2"
        (Bole.Tuple.String "v2") (List.assoc "value" row)
    | None -> Alcotest.fail "k2 not found after reload")

let tests =
  [ "repo", [
      Alcotest.test_case "init creates structure" `Quick test_init_creates_structure;
      Alcotest.test_case "round-trip" `Quick test_round_trip;
      Alcotest.test_case "branch persistence" `Quick test_branch_persistence;
    ]
  ]
