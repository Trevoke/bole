let test_round_trip () =
  let c = {
    Bole.Commit.state = Bole.Hash.hash "state";
    parents = [Bole.Hash.hash "parent1"; Bole.Hash.hash "parent2"];
    message = "test commit";
  } in
  let encoded = Bole.Commit.encode c in
  let decoded = Bole.Commit.decode encoded in
  Alcotest.(check bool) "state matches"
    true (Bole.Hash.equal c.state decoded.Bole.Commit.state);
  Alcotest.(check int) "parent count" 2 (List.length decoded.parents);
  Alcotest.(check bool) "parent1 matches"
    true (Bole.Hash.equal (List.nth c.parents 0) (List.nth decoded.parents 0));
  Alcotest.(check bool) "parent2 matches"
    true (Bole.Hash.equal (List.nth c.parents 1) (List.nth decoded.parents 1));
  Alcotest.(check string) "message matches" "test commit" decoded.message

let test_no_parents () =
  let c = {
    Bole.Commit.state = Bole.Hash.hash "state";
    parents = [];
    message = "initial";
  } in
  let decoded = Bole.Commit.decode (Bole.Commit.encode c) in
  Alcotest.(check int) "no parents" 0 (List.length decoded.Bole.Commit.parents)

let test_empty_message () =
  let c = {
    Bole.Commit.state = Bole.Hash.hash "state";
    parents = [];
    message = "";
  } in
  let decoded = Bole.Commit.decode (Bole.Commit.encode c) in
  Alcotest.(check string) "empty message" "" decoded.Bole.Commit.message

let tests =
  [ "commit", [
      Alcotest.test_case "round-trip" `Quick test_round_trip;
      Alcotest.test_case "no parents" `Quick test_no_parents;
      Alcotest.test_case "empty message" `Quick test_empty_message;
    ]
  ]
