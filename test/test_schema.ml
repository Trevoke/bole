let test_round_trip () =
  let s = Bole.Schema.create ~columns:["name", Bole.Schema.Str; "email", Bole.Schema.Str] in
  let decoded = Bole.Schema.decode (Bole.Schema.encode s) in
  Alcotest.(check int) "3 columns" 3 (List.length decoded.columns);
  Alcotest.(check string) "col 0 is _id" "_id" (fst (List.nth decoded.columns 0));
  Alcotest.(check string) "col 1" "name" (fst (List.nth decoded.columns 1));
  Alcotest.(check string) "pk is _id" "_id" (List.hd decoded.primary_key)

let test_auto_id () =
  let s = Bole.Schema.create ~columns:["x", Bole.Schema.Int64] in
  Alcotest.(check string) "first col is _id" "_id" (fst (List.hd s.columns));
  Alcotest.(check int) "pk is [_id]" 1 (List.length s.primary_key);
  Alcotest.(check string) "pk" "_id" (List.hd s.primary_key)

let test_key_value_columns () =
  let s = Bole.Schema.create ~columns:["name", Bole.Schema.Str; "email", Bole.Schema.Str] in
  let kc = Bole.Schema.key_columns s in
  let vc = Bole.Schema.value_columns s in
  Alcotest.(check int) "1 key col" 1 (List.length kc);
  Alcotest.(check string) "key is _id" "_id" (fst (List.hd kc));
  Alcotest.(check int) "2 value cols" 2 (List.length vc)

let test_multi_column () =
  let s = Bole.Schema.create ~columns:["region", Bole.Schema.Str; "id", Bole.Schema.Int64; "name", Bole.Schema.Str] in
  Alcotest.(check int) "4 columns (with _id)" 4 (List.length s.columns);
  let vc = Bole.Schema.value_columns s in
  Alcotest.(check int) "3 value cols" 3 (List.length vc)

let tests =
  [ "schema", [
      Alcotest.test_case "round-trip" `Quick test_round_trip;
      Alcotest.test_case "auto _id" `Quick test_auto_id;
      Alcotest.test_case "key/value columns" `Quick test_key_value_columns;
      Alcotest.test_case "multi-column" `Quick test_multi_column;
    ]
  ]
