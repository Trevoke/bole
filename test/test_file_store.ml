let with_tmp_dir f =
  let dir = Filename.temp_dir "bole-test" "" in
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

let test_file_put_get () =
  with_tmp_dir (fun dir ->
    let store = Bole.Store.create ~path:dir () in
    let data = "hello, file store" in
    let h = Bole.Store.put store data in
    let store2 = Bole.Store.create ~path:dir () in
    let got = Bole.Store.get store2 h in
    Alcotest.(check string) "round-trip via file" data got)

let test_file_mem () =
  with_tmp_dir (fun dir ->
    let store = Bole.Store.create ~path:dir () in
    let h = Bole.Store.put store "data" in
    let store2 = Bole.Store.create ~path:dir () in
    Alcotest.(check bool) "mem from file" true (Bole.Store.mem store2 h);
    let bogus = Bole.Hash.hash "never stored" in
    Alcotest.(check bool) "mem missing" false (Bole.Store.mem store2 bogus))

let test_file_idempotent () =
  with_tmp_dir (fun dir ->
    let store = Bole.Store.create ~path:dir () in
    let h1 = Bole.Store.put store "same" in
    let h2 = Bole.Store.put store "same" in
    let hash_testable =
      Alcotest.testable
        (fun fmt h -> Format.fprintf fmt "%s" (Bole.Hash.to_hex h))
        Bole.Hash.equal
    in
    Alcotest.(check hash_testable) "idempotent" h1 h2)

let test_inmemory_unchanged () =
  let store = Bole.Store.create () in
  let h = Bole.Store.put store "data" in
  let got = Bole.Store.get store h in
  Alcotest.(check string) "in-memory still works" "data" got

let tests =
  [ "file_store", [
      Alcotest.test_case "file put/get" `Quick test_file_put_get;
      Alcotest.test_case "file mem" `Quick test_file_mem;
      Alcotest.test_case "file idempotent" `Quick test_file_idempotent;
      Alcotest.test_case "in-memory unchanged" `Quick test_inmemory_unchanged;
    ]
  ]
