type t = {
  state : Hash.t;
  parents : Hash.t list;
  message : string;
}

let encode c =
  let buf = Buffer.create 256 in
  let add_u16 n =
    Buffer.add_char buf (Char.chr (n lsr 8 land 0xFF));
    Buffer.add_char buf (Char.chr (n land 0xFF))
  in
  Buffer.add_string buf (Hash.to_raw_string c.state);
  add_u16 (List.length c.parents);
  List.iter (fun h -> Buffer.add_string buf (Hash.to_raw_string h)) c.parents;
  add_u16 (String.length c.message);
  Buffer.add_string buf c.message;
  Buffer.contents buf

let decode data =
  let pos = ref 0 in
  let read_u16 () =
    let hi = Char.code data.[!pos] in
    let lo = Char.code data.[!pos + 1] in
    pos := !pos + 2;
    (hi lsl 8) lor lo
  in
  let state = Hash.of_raw_string (String.sub data !pos Hash.hash_size) in
  pos := !pos + Hash.hash_size;
  let parent_count = read_u16 () in
  let parents = List.init parent_count (fun _ ->
    let h = Hash.of_raw_string (String.sub data !pos Hash.hash_size) in
    pos := !pos + Hash.hash_size;
    h
  ) in
  let msg_len = read_u16 () in
  let message = String.sub data !pos msg_len in
  { state; parents; message }
