let test_round_trip () =
  let entries = [
    { Bole.Db_state.name = "posts"; root = Bole.Hash.hash "posts-root" };
    { Bole.Db_state.name = "users"; root = Bole.Hash.hash "users-root" };
  ] in
  let encoded = Bole.Db_state.encode entries in
  let decoded = Bole.Db_state.decode encoded in
  Alcotest.(check int) "same length" 2 (List.length decoded);
  Alcotest.(check string) "first name" "posts" (List.nth decoded 0).name;
  Alcotest.(check string) "second name" "users" (List.nth decoded 1).name;
  Alcotest.(check bool) "first root matches"
    true (Bole.Hash.equal (List.nth decoded 0).root (Bole.Hash.hash "posts-root"));
  Alcotest.(check bool) "second root matches"
    true (Bole.Hash.equal (List.nth decoded 1).root (Bole.Hash.hash "users-root"))

let test_empty_round_trip () =
  let encoded = Bole.Db_state.encode [] in
  let decoded = Bole.Db_state.decode encoded in
  Alcotest.(check int) "empty" 0 (List.length decoded)

let test_sorted_deterministic () =
  let entries_a = [
    { Bole.Db_state.name = "users"; root = Bole.Hash.hash "u" };
    { Bole.Db_state.name = "posts"; root = Bole.Hash.hash "p" };
  ] in
  let entries_b = [
    { Bole.Db_state.name = "posts"; root = Bole.Hash.hash "p" };
    { Bole.Db_state.name = "users"; root = Bole.Hash.hash "u" };
  ] in
  let a = Bole.Db_state.encode entries_a in
  let b = Bole.Db_state.encode entries_b in
  Alcotest.(check string) "sorted deterministic" a b

let tests =
  [ "db_state", [
      Alcotest.test_case "round-trip" `Quick test_round_trip;
      Alcotest.test_case "empty round-trip" `Quick test_empty_round_trip;
      Alcotest.test_case "sorted deterministic" `Quick test_sorted_deterministic;
    ]
  ]
