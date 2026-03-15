let test_v7_length () =
  let u = Bole.Uuid.v7 () in
  Alcotest.(check int) "16 bytes" 16 (String.length (Bole.Uuid.to_raw_string u))

let test_v7_unique () =
  let u1 = Bole.Uuid.v7 () in
  let u2 = Bole.Uuid.v7 () in
  Alcotest.(check bool) "unique" false (Bole.Uuid.equal u1 u2)

let test_v7_sortable () =
  let u1 = Bole.Uuid.v7 () in
  Unix.sleepf 0.002;
  let u2 = Bole.Uuid.v7 () in
  Alcotest.(check bool) "chronological order"
    true (Bole.Uuid.compare u1 u2 < 0)

let test_hex_round_trip () =
  let u = Bole.Uuid.v7 () in
  let hex = Bole.Uuid.to_hex u in
  Alcotest.(check int) "32 hex chars" 32 (String.length hex);
  let u2 = Bole.Uuid.of_hex hex in
  Alcotest.(check bool) "round-trip" true (Bole.Uuid.equal u u2)

let test_of_string_dashes () =
  let u = Bole.Uuid.v7 () in
  let hex = Bole.Uuid.to_hex u in
  let dashed = Printf.sprintf "%s-%s-%s-%s-%s"
    (String.sub hex 0 8) (String.sub hex 8 4) (String.sub hex 12 4)
    (String.sub hex 16 4) (String.sub hex 20 12) in
  let u2 = Bole.Uuid.of_string dashed in
  Alcotest.(check bool) "dashed round-trip" true (Bole.Uuid.equal u u2)

let test_of_hex_invalid () =
  (match Bole.Uuid.of_hex "tooshort" with
   | exception Invalid_argument _ -> ()
   | _ -> Alcotest.fail "expected Invalid_argument");
  (match Bole.Uuid.of_hex "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz" with
   | exception Invalid_argument _ -> ()
   | _ -> Alcotest.fail "expected Invalid_argument for invalid hex")

let tests =
  [ "uuid", [
      Alcotest.test_case "v7 length" `Quick test_v7_length;
      Alcotest.test_case "v7 unique" `Quick test_v7_unique;
      Alcotest.test_case "v7 sortable" `Quick test_v7_sortable;
      Alcotest.test_case "hex round-trip" `Quick test_hex_round_trip;
      Alcotest.test_case "of_string dashes" `Quick test_of_string_dashes;
      Alcotest.test_case "of_hex invalid" `Quick test_of_hex_invalid;
    ]
  ]
