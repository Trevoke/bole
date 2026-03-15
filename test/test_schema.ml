let test_round_trip () =
  let s = Bole.Schema.create
    ~columns:["id", Bole.Schema.Int64; "name", Bole.Schema.Str; "email", Bole.Schema.Str]
    ~primary_key:["id"] in
  let decoded = Bole.Schema.decode (Bole.Schema.encode s) in
  Alcotest.(check int) "3 columns" 3 (List.length decoded.columns);
  Alcotest.(check int) "1 pk" 1 (List.length decoded.primary_key);
  Alcotest.(check string) "pk is id" "id" (List.hd decoded.primary_key);
  Alcotest.(check string) "col 0" "id" (fst (List.nth decoded.columns 0));
  Alcotest.(check string) "col 1" "name" (fst (List.nth decoded.columns 1));
  Alcotest.(check string) "col 2" "email" (fst (List.nth decoded.columns 2))

let test_invalid_pk () =
  match Bole.Schema.create ~columns:["id", Bole.Schema.Int64] ~primary_key:["missing"] with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"

let test_key_value_columns () =
  let s = Bole.Schema.create
    ~columns:["id", Bole.Schema.Int64; "name", Bole.Schema.Str; "email", Bole.Schema.Str]
    ~primary_key:["id"] in
  let kc = Bole.Schema.key_columns s in
  let vc = Bole.Schema.value_columns s in
  Alcotest.(check int) "1 key col" 1 (List.length kc);
  Alcotest.(check int) "2 value cols" 2 (List.length vc);
  Alcotest.(check string) "key col is id" "id" (fst (List.hd kc));
  Alcotest.(check string) "val col 0" "name" (fst (List.nth vc 0));
  Alcotest.(check string) "val col 1" "email" (fst (List.nth vc 1))

let test_composite_pk () =
  let s = Bole.Schema.create
    ~columns:["region", Bole.Schema.Str; "id", Bole.Schema.Int64; "name", Bole.Schema.Str]
    ~primary_key:["region"; "id"] in
  let decoded = Bole.Schema.decode (Bole.Schema.encode s) in
  Alcotest.(check int) "2 pk cols" 2 (List.length decoded.primary_key);
  let kc = Bole.Schema.key_columns s in
  Alcotest.(check int) "2 key cols" 2 (List.length kc)

let tests =
  [ "schema", [
      Alcotest.test_case "round-trip" `Quick test_round_trip;
      Alcotest.test_case "invalid pk raises" `Quick test_invalid_pk;
      Alcotest.test_case "key/value columns" `Quick test_key_value_columns;
      Alcotest.test_case "composite pk" `Quick test_composite_pk;
    ]
  ]
