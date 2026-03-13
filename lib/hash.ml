type t = Digestif.BLAKE2S.t

let hash s = Digestif.BLAKE2S.digest_string s

let equal = Digestif.BLAKE2S.equal
let compare = Digestif.BLAKE2S.unsafe_compare

let to_raw_string h = Digestif.BLAKE2S.to_raw_string h
let of_raw_string s = Digestif.BLAKE2S.of_raw_string s

let to_hex h = Digestif.BLAKE2S.to_hex h

let hash_size = Digestif.BLAKE2S.digest_size
