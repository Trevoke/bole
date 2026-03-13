let hash_testable =
  Alcotest.testable
    (fun fmt h -> Format.fprintf fmt "%s" (Bole.Hash.to_hex h))
    Bole.Hash.equal

let test_deterministic () =
  let h1 = Bole.Hash.hash "hello" in
  let h2 = Bole.Hash.hash "hello" in
  Alcotest.(check hash_testable) "same input same hash" h1 h2

let test_different_inputs () =
  let h1 = Bole.Hash.hash "hello" in
  let h2 = Bole.Hash.hash "world" in
  Alcotest.(check bool) "different inputs different hash"
    false (Bole.Hash.equal h1 h2)

let test_hash_size () =
  let h = Bole.Hash.hash "test" in
  let raw = Bole.Hash.to_raw_string h in
  Alcotest.(check int) "raw digest is hash_size bytes"
    Bole.Hash.hash_size (String.length raw)

let test_hash_size_is_32 () =
  Alcotest.(check int) "hash_size is 32" 32 Bole.Hash.hash_size

let prop_round_trip =
  QCheck2.Test.make ~name:"to_raw_string / of_raw_string round-trip"
    QCheck2.Gen.string
    (fun s ->
       let h = Bole.Hash.hash s in
       Bole.Hash.equal h (Bole.Hash.of_raw_string (Bole.Hash.to_raw_string h)))

let tests =
  [ "hash", [
      Alcotest.test_case "deterministic" `Quick test_deterministic;
      Alcotest.test_case "different inputs" `Quick test_different_inputs;
      Alcotest.test_case "raw digest length" `Quick test_hash_size;
      Alcotest.test_case "hash_size is 32" `Quick test_hash_size_is_32;
      QCheck_alcotest.to_alcotest prop_round_trip;
    ]
  ]
