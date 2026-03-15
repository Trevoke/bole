type value =
  | Int64 of int64
  | String of string
  | Uuid of Uuid.t

type t = value list

let tag_int64 = '\x01'
let tag_string = '\x02'
let tag_uuid = '\x03'

let encode values =
  let buf = Buffer.create 64 in
  List.iter (fun v ->
    match v with
    | Int64 n ->
      Buffer.add_char buf tag_int64;
      let flipped = Int64.logxor n Int64.min_int in
      for i = 7 downto 0 do
        let byte = Int64.to_int (Int64.shift_right_logical flipped (i * 8)) land 0xFF in
        Buffer.add_char buf (Char.chr byte)
      done
    | String s ->
      Buffer.add_char buf tag_string;
      Buffer.add_string buf s;
      Buffer.add_char buf '\x00'
    | Uuid u ->
      Buffer.add_char buf tag_uuid;
      Buffer.add_string buf (Uuid.to_raw_string u)
  ) values;
  Buffer.contents buf

let decode data =
  let pos = ref 0 in
  let len = String.length data in
  let values = ref [] in
  while !pos < len do
    let tag = data.[!pos] in
    pos := !pos + 1;
    match tag with
    | c when c = tag_int64 ->
      let flipped = ref 0L in
      for i = 0 to 7 do
        let byte = Char.code data.[!pos + i] in
        flipped := Int64.logor (Int64.shift_left !flipped 8) (Int64.of_int byte)
      done;
      pos := !pos + 8;
      let n = Int64.logxor !flipped Int64.min_int in
      values := Int64 n :: !values
    | c when c = tag_string ->
      let start = !pos in
      while data.[!pos] <> '\x00' do
        pos := !pos + 1
      done;
      let s = String.sub data start (!pos - start) in
      pos := !pos + 1;
      values := String s :: !values
    | c when c = tag_uuid ->
      let raw = String.sub data !pos 16 in
      pos := !pos + 16;
      values := Uuid (Uuid.of_raw_string raw) :: !values
    | _ -> invalid_arg (Printf.sprintf "Tuple.decode: unknown tag 0x%02x" (Char.code tag))
  done;
  List.rev !values
