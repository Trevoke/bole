type t = string (* 16 raw bytes *)

let () = Random.self_init ()

let v7 () =
  let ts = Unix.gettimeofday () in
  let ms = Int64.of_float (ts *. 1000.0) in
  let buf = Bytes.create 16 in
  for i = 0 to 5 do
    let shift = (5 - i) * 8 in
    Bytes.set buf i (Char.chr (Int64.to_int (Int64.shift_right_logical ms shift) land 0xFF))
  done;
  for i = 6 to 15 do
    Bytes.set buf i (Char.chr (Random.bits () land 0xFF))
  done;
  let b6 = Char.code (Bytes.get buf 6) in
  Bytes.set buf 6 (Char.chr ((b6 land 0x0F) lor 0x70));
  let b8 = Char.code (Bytes.get buf 8) in
  Bytes.set buf 8 (Char.chr ((b8 land 0x3F) lor 0x80));
  Bytes.to_string buf

let equal = String.equal
let compare = String.compare
let to_raw_string t = t

let of_raw_string s =
  if String.length s <> 16 then
    invalid_arg (Printf.sprintf "Uuid.of_raw_string: expected 16 bytes, got %d" (String.length s));
  s

let to_hex t =
  let buf = Buffer.create 32 in
  String.iter (fun c ->
    let n = Char.code c in
    Buffer.add_char buf (Char.chr (let d = n lsr 4 in if d < 10 then d + 0x30 else d - 10 + 0x61));
    Buffer.add_char buf (Char.chr (let d = n land 0x0F in if d < 10 then d + 0x30 else d - 10 + 0x61))
  ) t;
  Buffer.contents buf

let hex_val c =
  if c >= '0' && c <= '9' then Char.code c - 0x30
  else if c >= 'a' && c <= 'f' then Char.code c - 0x61 + 10
  else if c >= 'A' && c <= 'F' then Char.code c - 0x41 + 10
  else invalid_arg "Uuid: invalid hex character"

let of_hex hex =
  if String.length hex <> 32 then
    invalid_arg (Printf.sprintf "Uuid.of_hex: expected 32 hex chars, got %d" (String.length hex));
  let raw = Bytes.create 16 in
  for i = 0 to 15 do
    let hi = hex_val hex.[i * 2] in
    let lo = hex_val hex.[i * 2 + 1] in
    Bytes.set raw i (Char.chr ((hi lsl 4) lor lo))
  done;
  Bytes.to_string raw

let of_string s =
  let stripped = String.concat "" (String.split_on_char '-' s) in
  of_hex stripped
