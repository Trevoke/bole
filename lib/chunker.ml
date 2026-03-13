type t = {
  target_size : int;
  min_size : int;
  level : int;
  mutable count : int;
}

let create ~target_size ~level =
  { target_size;
    min_size = max 1 (target_size / 4);
    level;
    count = 0 }

let feed t ~key =
  t.count <- t.count + 1;
  if t.count < t.min_size then false
  else
    let salted = String.make 1 (Char.chr (t.level land 0xFF)) ^ key in
    let h = Hash.hash salted in
    let raw = Hash.to_raw_string h in
    let low32 =
      let b0 = Char.code (String.get raw 0) in
      let b1 = Char.code (String.get raw 1) in
      let b2 = Char.code (String.get raw 2) in
      let b3 = Char.code (String.get raw 3) in
      ((b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3) land 0x7FFFFFFF
    in
    (* Dynamic probability: boundary likelihood increases quadratically
       with chunk size. P(boundary at count) = (count - min)² / target².

       At count = min_size:     probability 0%
       At count = target_size:  probability ~56% (for min = target/4)
       At count = 2*target:     forced boundary (100%)

       The quadratic ramp produces a tighter distribution than linear:
       the hazard rate starts very low and accelerates, so few chunks
       end early, most cluster near target_size, and the hard cap
       at 2x prevents runaways. *)
    if t.count >= t.target_size * 2 then
      true
    else
      let progress = t.count - t.min_size in
      let modulus = max 1 (t.target_size * t.target_size) in
      low32 mod modulus < progress * progress

let reset t = t.count <- 0

let count t = t.count
