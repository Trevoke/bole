module StringMap = Map.Make(String)

type t = {
  store : Store.t;
  branches : (string, Hash.t) Hashtbl.t; [@warning "-69"]
  current_branch : string; [@warning "-69"]
  tables : Hash.t StringMap.t;
} [@@warning "-69"]

type conflict = {
  table : string;
  key : string;
  base : string;
  ours : string;
  theirs : string;
}

type merge_result = {
  db : t;
  conflicts : conflict list;
}

let empty_tree_root store =
  Tree.build store Seq.empty

let create () = {
  store = Store.create ();
  branches = Hashtbl.create 16;
  current_branch = "main";
  tables = StringMap.empty;
}

let store db = db.store

let put db ~table ~key ~value =
  let root = match StringMap.find_opt table db.tables with
    | Some r -> r
    | None -> empty_tree_root db.store
  in
  let root' = Tree.put db.store root key value in
  { db with tables = StringMap.add table root' db.tables }

let delete db ~table ~key =
  let root = match StringMap.find_opt table db.tables with
    | Some r -> r
    | None -> raise Not_found
  in
  let root' = Tree.delete db.store root key in
  { db with tables = StringMap.add table root' db.tables }

let find db ~table ~key =
  match StringMap.find_opt table db.tables with
  | None -> None
  | Some root -> Tree.find db.store root key

let commit _db ~message:_ = failwith "not implemented"
let checkout _db _h = failwith "not implemented"
let parents _db _h = failwith "not implemented"
let branch _db ~name:_ = failwith "not implemented"
let switch _db ~name:_ = failwith "not implemented"
let diff _db ~from:_ ~to_:_ ~table:_ = failwith "not implemented"
let merge _db ~ours:_ ~theirs:_ = failwith "not implemented"
