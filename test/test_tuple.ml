let test_int64_round_trip () =
  let values = [Bole.Tuple.Int64 42L] in
  let encoded = Bole.Tuple.encode values in
  let decoded = Bole.Tuple.decode encoded in
  Alcotest.(check int) "one value" 1 (List.length decoded);
  match decoded with
  | [Bole.Tuple.Int64 42L] -> ()
  | _ -> Alcotest.fail "expected Int64 42"

let test_string_round_trip () =
  let values = [Bole.Tuple.String "hello"] in
  let decoded = Bole.Tuple.decode (Bole.Tuple.encode values) in
  match decoded with
  | [Bole.Tuple.String "hello"] -> ()
  | _ -> Alcotest.fail "expected String hello"

let test_uuid_round_trip () =
  let u = Bole.Uuid.v7 () in
  let values = [Bole.Tuple.Uuid u] in
  let decoded = Bole.Tuple.decode (Bole.Tuple.encode values) in
  match decoded with
  | [Bole.Tuple.Uuid u2] -> Alcotest.(check bool) "round-trip" true (Bole.Uuid.equal u u2)
  | _ -> Alcotest.fail "expected Uuid"

let test_multi_field_round_trip () =
  let u = Bole.Uuid.v7 () in
  let values = [Bole.Tuple.Uuid u; Bole.Tuple.String "alice"; Bole.Tuple.Int64 30L; Bole.Tuple.String "admin"] in
  let decoded = Bole.Tuple.decode (Bole.Tuple.encode values) in
  Alcotest.(check int) "four fields" 4 (List.length decoded);
  match decoded with
  | [Bole.Tuple.Uuid u2; Bole.Tuple.String "alice"; Bole.Tuple.Int64 30L; Bole.Tuple.String "admin"] ->
    Alcotest.(check bool) "uuid matches" true (Bole.Uuid.equal u u2)
  | _ -> Alcotest.fail "unexpected decoded values"

let test_empty_round_trip () =
  let encoded = Bole.Tuple.encode [] in
  let decoded = Bole.Tuple.decode encoded in
  Alcotest.(check int) "empty" 0 (List.length decoded)

let test_int64_ordering () =
  let encode_one n = Bole.Tuple.encode [Bole.Tuple.Int64 n] in
  let a = encode_one Int64.min_int in
  let b = encode_one (-1L) in
  let c = encode_one 0L in
  let d = encode_one 1L in
  let e = encode_one 100L in
  let f = encode_one Int64.max_int in
  Alcotest.(check bool) "min < -1" true (String.compare a b < 0);
  Alcotest.(check bool) "-1 < 0" true (String.compare b c < 0);
  Alcotest.(check bool) "0 < 1" true (String.compare c d < 0);
  Alcotest.(check bool) "1 < 100" true (String.compare d e < 0);
  Alcotest.(check bool) "100 < max" true (String.compare e f < 0)

let test_string_ordering () =
  let encode_one s = Bole.Tuple.encode [Bole.Tuple.String s] in
  let a = encode_one "" in
  let b = encode_one "ab" in
  let c = encode_one "abc" in
  let d = encode_one "b" in
  Alcotest.(check bool) "empty < ab" true (String.compare a b < 0);
  Alcotest.(check bool) "ab < abc" true (String.compare b c < 0);
  Alcotest.(check bool) "abc < b" true (String.compare c d < 0)

let test_uuid_ordering () =
  let u1 = Bole.Uuid.v7 () in
  Unix.sleepf 0.002;
  let u2 = Bole.Uuid.v7 () in
  let a = Bole.Tuple.encode [Bole.Tuple.Uuid u1] in
  let b = Bole.Tuple.encode [Bole.Tuple.Uuid u2] in
  Alcotest.(check bool) "uuid1 < uuid2" true (String.compare a b < 0)

let test_composite_ordering () =
  let encode_pair s n = Bole.Tuple.encode [Bole.Tuple.String s; Bole.Tuple.Int64 n] in
  let a = encode_pair "alice" 10L in
  let b = encode_pair "alice" 20L in
  let c = encode_pair "bob" 1L in
  Alcotest.(check bool) "alice,10 < alice,20" true (String.compare a b < 0);
  Alcotest.(check bool) "alice,20 < bob,1" true (String.compare b c < 0)

(* --- New tests: byte-stuffing for String --- *)

let test_string_with_null () =
  let s = "hel\x00lo" in
  let values = [Bole.Tuple.String s] in
  let decoded = Bole.Tuple.decode (Bole.Tuple.encode values) in
  match decoded with
  | [Bole.Tuple.String s2] ->
    Alcotest.(check string) "round-trip with null" s s2
  | _ -> Alcotest.fail "expected String"

let test_string_all_nulls () =
  let s = "\x00\x00\x00" in
  let values = [Bole.Tuple.String s] in
  let decoded = Bole.Tuple.decode (Bole.Tuple.encode values) in
  match decoded with
  | [Bole.Tuple.String s2] ->
    Alcotest.(check string) "round-trip all nulls" s s2
  | _ -> Alcotest.fail "expected String"

(* --- New tests: Bool --- *)

let test_bool_round_trip () =
  let values = [Bole.Tuple.Bool true; Bole.Tuple.Bool false] in
  let decoded = Bole.Tuple.decode (Bole.Tuple.encode values) in
  match decoded with
  | [Bole.Tuple.Bool true; Bole.Tuple.Bool false] -> ()
  | _ -> Alcotest.fail "expected Bool true; Bool false"

let test_bool_ordering () =
  let a = Bole.Tuple.encode [Bole.Tuple.Bool false] in
  let b = Bole.Tuple.encode [Bole.Tuple.Bool true] in
  Alcotest.(check bool) "false < true" true (String.compare a b < 0)

(* --- New tests: Float --- *)

let test_float_round_trip () =
  let values = [Bole.Tuple.Float 3.14; Bole.Tuple.Float (-2.718)] in
  let decoded = Bole.Tuple.decode (Bole.Tuple.encode values) in
  match decoded with
  | [Bole.Tuple.Float a; Bole.Tuple.Float b] ->
    Alcotest.(check (float 0.0)) "positive" 3.14 a;
    Alcotest.(check (float 0.0)) "negative" (-2.718) b
  | _ -> Alcotest.fail "expected two Floats"

let test_float_ordering () =
  let encode_one f = Bole.Tuple.encode [Bole.Tuple.Float f] in
  let a = encode_one neg_infinity in
  let b = encode_one (-1.0) in
  let c = encode_one (-0.0) in
  let d = encode_one 0.0 in
  let e = encode_one 1.0 in
  let f = encode_one infinity in
  Alcotest.(check bool) "-inf < -1" true (String.compare a b < 0);
  Alcotest.(check bool) "-1 < -0" true (String.compare b c < 0);
  Alcotest.(check bool) "-0 < +0" true (String.compare c d < 0);
  Alcotest.(check bool) "+0 < +1" true (String.compare d e < 0);
  Alcotest.(check bool) "+1 < +inf" true (String.compare e f < 0)

let test_float_nan_rejected () =
  match Bole.Tuple.encode [Bole.Tuple.Float nan] with
  | _ -> Alcotest.fail "expected Invalid_argument"
  | exception Invalid_argument _ -> ()

(* --- New tests: Timestamp --- *)

let test_timestamp_round_trip () =
  let values = [Bole.Tuple.Timestamp 1710000000000L] in
  let decoded = Bole.Tuple.decode (Bole.Tuple.encode values) in
  match decoded with
  | [Bole.Tuple.Timestamp 1710000000000L] -> ()
  | _ -> Alcotest.fail "expected Timestamp"

let test_timestamp_ordering () =
  let a = Bole.Tuple.encode [Bole.Tuple.Timestamp 1000L] in
  let b = Bole.Tuple.encode [Bole.Tuple.Timestamp 2000L] in
  Alcotest.(check bool) "earlier < later" true (String.compare a b < 0)

(* --- New tests: Blob --- *)

let test_blob_round_trip () =
  let s = "\x00\x01\x02\x00\xFF" in
  let values = [Bole.Tuple.Blob s] in
  let decoded = Bole.Tuple.decode (Bole.Tuple.encode values) in
  match decoded with
  | [Bole.Tuple.Blob s2] ->
    Alcotest.(check string) "blob round-trip" s s2
  | _ -> Alcotest.fail "expected Blob"

let test_blob_ordering () =
  let encode_one s = Bole.Tuple.encode [Bole.Tuple.Blob s] in
  let a = encode_one "abc" in
  let b = encode_one "abd" in
  let c = encode_one "b" in
  Alcotest.(check bool) "abc < abd" true (String.compare a b < 0);
  Alcotest.(check bool) "abd < b" true (String.compare b c < 0)

let tests =
  [ "tuple", [
      Alcotest.test_case "int64 round-trip" `Quick test_int64_round_trip;
      Alcotest.test_case "string round-trip" `Quick test_string_round_trip;
      Alcotest.test_case "uuid round-trip" `Quick test_uuid_round_trip;
      Alcotest.test_case "multi-field round-trip" `Quick test_multi_field_round_trip;
      Alcotest.test_case "empty round-trip" `Quick test_empty_round_trip;
      Alcotest.test_case "int64 ordering" `Quick test_int64_ordering;
      Alcotest.test_case "string ordering" `Quick test_string_ordering;
      Alcotest.test_case "uuid ordering" `Quick test_uuid_ordering;
      Alcotest.test_case "composite ordering" `Quick test_composite_ordering;
      Alcotest.test_case "string with null" `Quick test_string_with_null;
      Alcotest.test_case "string all nulls" `Quick test_string_all_nulls;
      Alcotest.test_case "bool round-trip" `Quick test_bool_round_trip;
      Alcotest.test_case "bool ordering" `Quick test_bool_ordering;
      Alcotest.test_case "float round-trip" `Quick test_float_round_trip;
      Alcotest.test_case "float ordering" `Quick test_float_ordering;
      Alcotest.test_case "float nan rejected" `Quick test_float_nan_rejected;
      Alcotest.test_case "timestamp round-trip" `Quick test_timestamp_round_trip;
      Alcotest.test_case "timestamp ordering" `Quick test_timestamp_ordering;
      Alcotest.test_case "blob round-trip" `Quick test_blob_round_trip;
      Alcotest.test_case "blob ordering" `Quick test_blob_ordering;
    ]
  ]
