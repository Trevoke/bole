module Tbl = Hashtbl.Make (struct
  type t = Hash.t
  let equal = Hash.equal
  let hash h = Hashtbl.hash (Hash.to_raw_string h)
end)

type t = {
  tbl : string Tbl.t;
  mutable gets : int;
  mutable puts : int;
}

let create () = { tbl = Tbl.create 1024; gets = 0; puts = 0 }

let put store data =
  store.puts <- store.puts + 1;
  let h = Hash.hash data in
  if not (Tbl.mem store.tbl h) then
    Tbl.replace store.tbl h data;
  h

let get store h =
  store.gets <- store.gets + 1;
  Tbl.find store.tbl h

let mem store h =
  Tbl.mem store.tbl h

let get_count store = store.gets
let put_count store = store.puts
let reset_stats store =
  store.gets <- 0;
  store.puts <- 0
