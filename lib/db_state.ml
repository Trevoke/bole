type table_entry = { name : string; root : Hash.t }

type t = table_entry list

let encode entries =
  let sorted = List.sort (fun a b -> String.compare a.name b.name) entries in
  let buf = Buffer.create 256 in
  let add_u16 n =
    Buffer.add_char buf (Char.chr (n lsr 8 land 0xFF));
    Buffer.add_char buf (Char.chr (n land 0xFF))
  in
  add_u16 (List.length sorted);
  List.iter (fun e ->
    add_u16 (String.length e.name);
    Buffer.add_string buf e.name;
    Buffer.add_string buf (Hash.to_raw_string e.root)
  ) sorted;
  Buffer.contents buf

let decode data =
  let pos = ref 0 in
  let read_u16 () =
    let hi = Char.code data.[!pos] in
    let lo = Char.code data.[!pos + 1] in
    pos := !pos + 2;
    (hi lsl 8) lor lo
  in
  let count = read_u16 () in
  let entries = List.init count (fun _ ->
    let name_len = read_u16 () in
    let name = String.sub data !pos name_len in
    pos := !pos + name_len;
    let root = Hash.of_raw_string (String.sub data !pos Hash.hash_size) in
    pos := !pos + Hash.hash_size;
    { name; root }
  ) in
  entries
