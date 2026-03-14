module Tbl = Hashtbl.Make (struct
  type t = Hash.t
  let equal = Hash.equal
  let hash h = Hashtbl.hash (Hash.to_raw_string h)
end)

type t = {
  tbl : string Tbl.t;
  mutable gets : int;
  mutable puts : int;
  path : string option;
}

let create ?path () = { tbl = Tbl.create 1024; gets = 0; puts = 0; path }

let object_path dir hex =
  let prefix = String.sub hex 0 2 in
  let suffix = String.sub hex 2 (String.length hex - 2) in
  Filename.concat (Filename.concat dir prefix) suffix

let write_file path data =
  let dir = Filename.dirname path in
  if not (Sys.file_exists dir) then
    Sys.mkdir dir 0o755;
  let oc = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () ->
    output_string oc data)

let read_file path =
  let ic = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
    let len = in_channel_length ic in
    really_input_string ic len)

let put store data =
  store.puts <- store.puts + 1;
  let h = Hash.hash data in
  if not (Tbl.mem store.tbl h) then begin
    Tbl.replace store.tbl h data;
    match store.path with
    | Some dir ->
      let file = object_path dir (Hash.to_hex h) in
      if not (Sys.file_exists file) then
        write_file file data
    | None -> ()
  end;
  h

let get store h =
  store.gets <- store.gets + 1;
  match Tbl.find_opt store.tbl h with
  | Some data -> data
  | None ->
    match store.path with
    | Some dir ->
      let file = object_path dir (Hash.to_hex h) in
      let data = read_file file in
      Tbl.replace store.tbl h data;
      data
    | None -> raise Not_found

let mem store h =
  Tbl.mem store.tbl h ||
  match store.path with
  | Some dir ->
    let file = object_path dir (Hash.to_hex h) in
    Sys.file_exists file
  | None -> false

let get_count store = store.gets
let put_count store = store.puts
let reset_stats store =
  store.gets <- 0;
  store.puts <- 0
