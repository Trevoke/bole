type leaf_entry = { key : string; value : string }

type internal_entry = { key : string; child : Hash.t }

type t =
  | Leaf of leaf_entry list
  | Internal of internal_entry list

let fail_truncated () =
  invalid_arg "Chunk.decode: unexpected end of data"

(* --- Encoding --- *)

let encode_uint16_be buf n =
  Buffer.add_char buf (Char.chr ((n lsr 8) land 0xFF));
  Buffer.add_char buf (Char.chr (n land 0xFF))

let encode_leaf_entry buf { key; value } =
  encode_uint16_be buf (String.length key);
  Buffer.add_string buf key;
  encode_uint16_be buf (String.length value);
  Buffer.add_string buf value

let encode_internal_entry buf { key; child } =
  encode_uint16_be buf (String.length key);
  Buffer.add_string buf key;
  Buffer.add_string buf (Hash.to_raw_string child)

let encode = function
  | Leaf entries ->
    let buf = Buffer.create 256 in
    Buffer.add_char buf '\x00';
    List.iter (encode_leaf_entry buf) entries;
    Buffer.contents buf
  | Internal entries ->
    let buf = Buffer.create 256 in
    Buffer.add_char buf '\x01';
    List.iter (encode_internal_entry buf) entries;
    Buffer.contents buf

(* --- Decoding --- *)

let decode_uint16_be data off =
  if off + 2 > String.length data then fail_truncated ();
  let b0 = Char.code (String.get data off) in
  let b1 = Char.code (String.get data (off + 1)) in
  ((b0 lsl 8) lor b1, off + 2)

let decode_bytes data off len =
  if off + len > String.length data then fail_truncated ();
  (String.sub data off len, off + len)

let decode_leaf_entries data off =
  let len = String.length data in
  let entries = ref [] in
  let pos = ref off in
  while !pos < len do
    let (key_len, p) = decode_uint16_be data !pos in
    let (key, p) = decode_bytes data p key_len in
    let (val_len, p) = decode_uint16_be data p in
    let (value, p) = decode_bytes data p val_len in
    entries := { key; value } :: !entries;
    pos := p
  done;
  List.rev !entries

let decode_internal_entries data off =
  let len = String.length data in
  let entries = ref [] in
  let pos = ref off in
  while !pos < len do
    let (key_len, p) = decode_uint16_be data !pos in
    let (key, p) = decode_bytes data p key_len in
    let (hash_raw, p) = decode_bytes data p Hash.hash_size in
    let child = Hash.of_raw_string hash_raw in
    entries := { key; child } :: !entries;
    pos := p
  done;
  List.rev !entries

let decode data =
  if String.length data < 1 then
    invalid_arg "Chunk.decode: unexpected end of data";
  match Char.code (String.get data 0) with
  | 0x00 -> Leaf (decode_leaf_entries data 1)
  | 0x01 -> Internal (decode_internal_entries data 1)
  | n -> invalid_arg (Printf.sprintf "Chunk.decode: unknown node type 0x%02X" n)
