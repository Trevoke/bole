type value =
  | Int64 of int64
  | String of string
  | Uuid of Uuid.t
  | Bool of bool
  | Float of float
  | Timestamp of int64
  | Blob of string

type t = value list

let tag_int64 = '\x01'
let tag_string = '\x02'
let tag_uuid = '\x03'
let tag_bool = '\x04'
let tag_float = '\x05'
let tag_timestamp = '\x06'
let tag_blob = '\x07'

let encode_byte_stuffed buf s =
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    if c = '\x00' then begin
      Buffer.add_char buf '\x00';
      Buffer.add_char buf '\xFF'
    end else
      Buffer.add_char buf c
  done;
  Buffer.add_char buf '\x00';
  Buffer.add_char buf '\x00'

let decode_byte_stuffed data pos =
  let buf = Buffer.create 64 in
  let continue = ref true in
  while !continue do
    let c = data.[!pos] in
    if c = '\x00' then begin
      let next = data.[!pos + 1] in
      if next = '\x00' then begin
        pos := !pos + 2;
        continue := false
      end else if next = '\xFF' then begin
        Buffer.add_char buf '\x00';
        pos := !pos + 2
      end else
        invalid_arg "Tuple.decode: invalid byte-stuffing sequence"
    end else begin
      Buffer.add_char buf c;
      pos := !pos + 1
    end
  done;
  Buffer.contents buf

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
      encode_byte_stuffed buf s
    | Uuid u ->
      Buffer.add_char buf tag_uuid;
      Buffer.add_string buf (Uuid.to_raw_string u)
    | Bool b ->
      Buffer.add_char buf tag_bool;
      Buffer.add_char buf (if b then '\x01' else '\x00')
    | Float f ->
      if Float.is_nan f then invalid_arg "Tuple.encode: NaN not allowed";
      Buffer.add_char buf tag_float;
      let bits = Int64.bits_of_float f in
      let encoded =
        if Int64.shift_right_logical bits 63 = 1L then
          Int64.lognot bits
        else
          Int64.logxor bits Int64.min_int
      in
      for i = 7 downto 0 do
        let byte = Int64.to_int (Int64.shift_right_logical encoded (i * 8)) land 0xFF in
        Buffer.add_char buf (Char.chr byte)
      done
    | Timestamp n ->
      Buffer.add_char buf tag_timestamp;
      let flipped = Int64.logxor n Int64.min_int in
      for i = 7 downto 0 do
        let byte = Int64.to_int (Int64.shift_right_logical flipped (i * 8)) land 0xFF in
        Buffer.add_char buf (Char.chr byte)
      done
    | Blob s ->
      Buffer.add_char buf tag_blob;
      encode_byte_stuffed buf s
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
      let s = decode_byte_stuffed data pos in
      values := String s :: !values
    | c when c = tag_uuid ->
      let raw = String.sub data !pos 16 in
      pos := !pos + 16;
      values := Uuid (Uuid.of_raw_string raw) :: !values
    | c when c = tag_bool ->
      let b = data.[!pos] <> '\x00' in
      pos := !pos + 1;
      values := Bool b :: !values
    | c when c = tag_float ->
      let encoded = ref 0L in
      for i = 0 to 7 do
        let byte = Char.code data.[!pos + i] in
        encoded := Int64.logor (Int64.shift_left !encoded 8) (Int64.of_int byte)
      done;
      pos := !pos + 8;
      let bits =
        if Int64.shift_right_logical !encoded 63 = 1L then
          Int64.logxor !encoded Int64.min_int
        else
          Int64.lognot !encoded
      in
      values := Float (Int64.float_of_bits bits) :: !values
    | c when c = tag_timestamp ->
      let flipped = ref 0L in
      for i = 0 to 7 do
        let byte = Char.code data.[!pos + i] in
        flipped := Int64.logor (Int64.shift_left !flipped 8) (Int64.of_int byte)
      done;
      pos := !pos + 8;
      let n = Int64.logxor !flipped Int64.min_int in
      values := Timestamp n :: !values
    | c when c = tag_blob ->
      let s = decode_byte_stuffed data pos in
      values := Blob s :: !values
    | _ -> invalid_arg (Printf.sprintf "Tuple.decode: unknown tag 0x%02x" (Char.code tag))
  done;
  List.rev !values
