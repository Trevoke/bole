let test_put_get_round_trip () =
  let store = Bole.Store.create () in
  let data = "hello, chunks" in
  let h = Bole.Store.put store data in
  let got = Bole.Store.get store h in
  Alcotest.(check string) "round-trip" data got

let test_put_idempotent () =
  let store = Bole.Store.create () in
  let h1 = Bole.Store.put store "same data" in
  let h2 = Bole.Store.put store "same data" in
  let hash_testable =
    Alcotest.testable
      (fun fmt h -> Format.fprintf fmt "%s" (Bole.Hash.to_hex h))
      Bole.Hash.equal
  in
  Alcotest.(check hash_testable) "same hash" h1 h2

let test_mem_after_put () =
  let store = Bole.Store.create () in
  let h = Bole.Store.put store "data" in
  Alcotest.(check bool) "mem after put" true (Bole.Store.mem store h)

let test_mem_unknown () =
  let store = Bole.Store.create () in
  let bogus = Bole.Hash.hash "never stored" in
  Alcotest.(check bool) "mem unknown" false (Bole.Store.mem store bogus)

let test_get_unknown_raises () =
  let store = Bole.Store.create () in
  let bogus = Bole.Hash.hash "never stored" in
  Alcotest.check_raises "get unknown" Not_found
    (fun () -> ignore (Bole.Store.get store bogus))

let prop_round_trip =
  QCheck2.Test.make ~name:"put/get round-trip for arbitrary data"
    QCheck2.Gen.string
    (fun data ->
       let store = Bole.Store.create () in
       let h = Bole.Store.put store data in
       String.equal data (Bole.Store.get store h))

let prop_content_addressing =
  QCheck2.Test.make ~name:"same data always yields same hash"
    QCheck2.Gen.string
    (fun data ->
       let s1 = Bole.Store.create () in
       let s2 = Bole.Store.create () in
       let h1 = Bole.Store.put s1 data in
       let h2 = Bole.Store.put s2 data in
       Bole.Hash.equal h1 h2)

let tests =
  [ "store", [
      Alcotest.test_case "put/get round-trip" `Quick test_put_get_round_trip;
      Alcotest.test_case "put idempotent" `Quick test_put_idempotent;
      Alcotest.test_case "mem after put" `Quick test_mem_after_put;
      Alcotest.test_case "mem unknown" `Quick test_mem_unknown;
      Alcotest.test_case "get unknown raises" `Quick test_get_unknown_raises;
      QCheck_alcotest.to_alcotest prop_round_trip;
      QCheck_alcotest.to_alcotest prop_content_addressing;
    ]
  ]
