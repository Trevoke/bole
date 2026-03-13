type t = {
  target_size : int;
  min_size : int;
  mutable count : int;
}

let create ~target_size =
  { target_size;
    min_size = max 1 (target_size / 4);
    count = 0 }

let feed t ~key =
  t.count <- t.count + 1;
  if t.count < t.min_size then false
  else
    let h = Hash.hash key in
    let raw = Hash.to_raw_string h in
    let low32 =
      let b0 = Char.code (String.get raw 0) in
      let b1 = Char.code (String.get raw 1) in
      let b2 = Char.code (String.get raw 2) in
      let b3 = Char.code (String.get raw 3) in
      ((b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3) land 0x7FFFFFFF
    in
    (* After min_size, use modular check with a denominator that
       yields mean chunk size ≈ target_size.
       Expected keys after min_size = target_size - min_size,
       so probability per key = 1 / (target_size - min_size).
       We use: low32 mod denominator = 0, where denominator = target - min. *)
    let base_denom = max 1 (t.target_size - t.min_size) in
    if t.count >= t.target_size * 2 then
      (* Force boundary at 2x target *)
      true
    else
      low32 mod base_denom = 0

let reset t = t.count <- 0

let count t = t.count
