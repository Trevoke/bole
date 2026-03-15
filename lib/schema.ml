type column_type = Int64 | Str

type t = {
  columns : (string * column_type) list;
  primary_key : string list;
}

let create ~columns ~primary_key =
  List.iter (fun pk ->
    if not (List.mem_assoc pk columns) then
      invalid_arg (Printf.sprintf "Schema.create: primary key column %S not in columns" pk)
  ) primary_key;
  { columns; primary_key }

let key_columns schema =
  List.filter (fun (name, _) -> List.mem name schema.primary_key) schema.columns

let value_columns schema =
  List.filter (fun (name, _) -> not (List.mem name schema.primary_key)) schema.columns

let type_to_byte = function
  | Int64 -> '\x01'
  | Str -> '\x02'

let byte_to_type = function
  | '\x01' -> Int64
  | '\x02' -> Str
  | c -> invalid_arg (Printf.sprintf "Schema.decode: unknown type byte 0x%02x" (Char.code c))

let encode schema =
  let buf = Buffer.create 128 in
  let add_u16 n =
    Buffer.add_char buf (Char.chr (n lsr 8 land 0xFF));
    Buffer.add_char buf (Char.chr (n land 0xFF))
  in
  add_u16 (List.length schema.columns);
  List.iter (fun (name, typ) ->
    add_u16 (String.length name);
    Buffer.add_string buf name;
    Buffer.add_char buf (type_to_byte typ)
  ) schema.columns;
  add_u16 (List.length schema.primary_key);
  List.iter (fun name ->
    add_u16 (String.length name);
    Buffer.add_string buf name
  ) schema.primary_key;
  Buffer.contents buf

let decode data =
  let pos = ref 0 in
  let read_u16 () =
    let hi = Char.code data.[!pos] in
    let lo = Char.code data.[!pos + 1] in
    pos := !pos + 2;
    (hi lsl 8) lor lo
  in
  let col_count = read_u16 () in
  let columns = List.init col_count (fun _ ->
    let name_len = read_u16 () in
    let name = String.sub data !pos name_len in
    pos := !pos + name_len;
    let typ = byte_to_type data.[!pos] in
    pos := !pos + 1;
    (name, typ)
  ) in
  let pk_count = read_u16 () in
  let primary_key = List.init pk_count (fun _ ->
    let name_len = read_u16 () in
    let name = String.sub data !pos name_len in
    pos := !pos + name_len;
    name
  ) in
  { columns; primary_key }
