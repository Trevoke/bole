module Tbl = Hashtbl.Make (struct
  type t = Hash.t
  let equal = Hash.equal
  let hash h = Hashtbl.hash (Hash.to_raw_string h)
end)

type t = { tbl : string Tbl.t }

let create () = { tbl = Tbl.create 1024 }

let put store data =
  let h = Hash.hash data in
  if not (Tbl.mem store.tbl h) then
    Tbl.replace store.tbl h data;
  h

let get store h =
  Tbl.find store.tbl h

let mem store h =
  Tbl.mem store.tbl h
