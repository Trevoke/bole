let leaf_entry_equal (a : Bole.Chunk.leaf_entry) (b : Bole.Chunk.leaf_entry) =
  String.equal a.key b.key
  && String.equal a.value b.value

let internal_entry_equal a b =
  String.equal a.Bole.Chunk.key b.Bole.Chunk.key
  && Bole.Hash.equal a.Bole.Chunk.child b.Bole.Chunk.child

let chunk_equal a b =
  match a, b with
  | Bole.Chunk.Leaf a_entries, Bole.Chunk.Leaf b_entries ->
    List.length a_entries = List.length b_entries
    && List.for_all2 leaf_entry_equal a_entries b_entries
  | Bole.Chunk.Internal a_entries, Bole.Chunk.Internal b_entries ->
    List.length a_entries = List.length b_entries
    && List.for_all2 internal_entry_equal a_entries b_entries
  | _ -> false

let chunk_pp fmt = function
  | Bole.Chunk.Leaf entries ->
    Format.fprintf fmt "Leaf[%d entries]" (List.length entries)
  | Bole.Chunk.Internal entries ->
    Format.fprintf fmt "Internal[%d entries]" (List.length entries)

let chunk_testable = Alcotest.testable chunk_pp chunk_equal

let test_leaf_round_trip () =
  let chunk = Bole.Chunk.Leaf [
    { key = "alice"; value = "v1" };
    { key = "bob"; value = "v2" };
  ] in
  let encoded = Bole.Chunk.encode chunk in
  let decoded = Bole.Chunk.decode encoded in
  Alcotest.(check chunk_testable) "leaf round-trip" chunk decoded

let test_internal_round_trip () =
  let h1 = Bole.Hash.hash "child1" in
  let h2 = Bole.Hash.hash "child2" in
  let chunk = Bole.Chunk.Internal [
    { key = "alice"; child = h1 };
    { key = "bob"; child = h2 };
  ] in
  let encoded = Bole.Chunk.encode chunk in
  let decoded = Bole.Chunk.decode encoded in
  Alcotest.(check chunk_testable) "internal round-trip" chunk decoded

let test_empty_leaf_round_trip () =
  let chunk = Bole.Chunk.Leaf [] in
  let encoded = Bole.Chunk.encode chunk in
  let decoded = Bole.Chunk.decode encoded in
  Alcotest.(check chunk_testable) "empty leaf round-trip" chunk decoded

let test_empty_internal_round_trip () =
  let chunk = Bole.Chunk.Internal [] in
  let encoded = Bole.Chunk.encode chunk in
  let decoded = Bole.Chunk.decode encoded in
  Alcotest.(check chunk_testable) "empty internal round-trip" chunk decoded

let test_leaf_node_type_byte () =
  let chunk = Bole.Chunk.Leaf [] in
  let encoded = Bole.Chunk.encode chunk in
  Alcotest.(check int) "leaf type byte is 0x00"
    0x00 (Char.code (String.get encoded 0))

let test_internal_node_type_byte () =
  let chunk = Bole.Chunk.Internal [] in
  let encoded = Bole.Chunk.encode chunk in
  Alcotest.(check int) "internal type byte is 0x01"
    0x01 (Char.code (String.get encoded 0))

let test_known_leaf_encoding () =
  (* Hand-compute: type=0x00, key "ab" len=0x0002, val "c" len=0x0001 *)
  let chunk = Bole.Chunk.Leaf [{ key = "ab"; value = "c" }] in
  let encoded = Bole.Chunk.encode chunk in
  let expected = "\x00\x00\x02ab\x00\x01c" in
  Alcotest.(check string) "known encoding" expected encoded

let test_decode_truncated_raises () =
  Alcotest.check_raises "truncated data"
    (Invalid_argument "Chunk.decode: unexpected end of data")
    (fun () -> ignore (Bole.Chunk.decode "\x00\x00\x05ab"))

let gen_leaf_entry =
  QCheck2.Gen.(
    let+ key = string_small_of printable
    and+ value = string_small_of printable in
    Bole.Chunk.{ key; value })

let gen_internal_entry =
  QCheck2.Gen.(
    let+ key = string_small_of printable
    and+ child_input = string_small_of printable in
    Bole.Chunk.{ key; child = Bole.Hash.hash child_input })

let prop_leaf_round_trip =
  QCheck2.Test.make ~name:"leaf round-trip (arbitrary)"
    QCheck2.Gen.(list_size (int_range 0 50) gen_leaf_entry)
    (fun entries ->
       let chunk = Bole.Chunk.Leaf entries in
       chunk_equal chunk (Bole.Chunk.decode (Bole.Chunk.encode chunk)))

let prop_internal_round_trip =
  QCheck2.Test.make ~name:"internal round-trip (arbitrary)"
    QCheck2.Gen.(list_size (int_range 0 50) gen_internal_entry)
    (fun entries ->
       let chunk = Bole.Chunk.Internal entries in
       chunk_equal chunk (Bole.Chunk.decode (Bole.Chunk.encode chunk)))

let tests =
  [ "chunk", [
      Alcotest.test_case "leaf round-trip" `Quick test_leaf_round_trip;
      Alcotest.test_case "internal round-trip" `Quick test_internal_round_trip;
      Alcotest.test_case "empty leaf round-trip" `Quick test_empty_leaf_round_trip;
      Alcotest.test_case "empty internal round-trip" `Quick test_empty_internal_round_trip;
      Alcotest.test_case "leaf type byte" `Quick test_leaf_node_type_byte;
      Alcotest.test_case "internal type byte" `Quick test_internal_node_type_byte;
      Alcotest.test_case "known encoding" `Quick test_known_leaf_encoding;
      Alcotest.test_case "truncated data raises" `Quick test_decode_truncated_raises;
      QCheck_alcotest.to_alcotest prop_leaf_round_trip;
      QCheck_alcotest.to_alcotest prop_internal_round_trip;
    ]
  ]
