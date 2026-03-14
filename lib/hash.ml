type t = Digestif.BLAKE2S.t

let hash s = Digestif.BLAKE2S.digest_string s

let equal = Digestif.BLAKE2S.equal
let compare = Digestif.BLAKE2S.unsafe_compare

let to_raw_string h = Digestif.BLAKE2S.to_raw_string h
let of_raw_string s = Digestif.BLAKE2S.of_raw_string s

let to_hex h = Digestif.BLAKE2S.to_hex h

let hash_size = Digestif.BLAKE2S.digest_size

let of_hex hex =
  let len = String.length hex in
  if len <> hash_size * 2 then
    invalid_arg (Printf.sprintf "Hash.of_hex: expected %d hex chars, got %d" (hash_size * 2) len);
  let raw = Bytes.create hash_size in
  for i = 0 to hash_size - 1 do
    let hi = Char.code hex.[i * 2] in
    let lo = Char.code hex.[i * 2 + 1] in
    let hex_val c =
      if c >= 0x30 && c <= 0x39 then c - 0x30
      else if c >= 0x61 && c <= 0x66 then c - 0x61 + 10
      else if c >= 0x41 && c <= 0x46 then c - 0x41 + 10
      else invalid_arg "Hash.of_hex: invalid hex character"
    in
    Bytes.set raw i (Char.chr ((hex_val hi lsl 4) lor hex_val lo))
  done;
  of_raw_string (Bytes.to_string raw)
