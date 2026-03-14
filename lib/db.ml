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

let commit db ~message =
  let entries = StringMap.fold (fun name root acc ->
    Db_state.{ name; root } :: acc
  ) db.tables [] in
  let state_data = Db_state.encode entries in
  let state_hash = Store.put db.store state_data in
  let parents = match Hashtbl.find_opt db.branches db.current_branch with
    | Some h -> [h]
    | None -> []
  in
  let commit_obj = Commit.{ state = state_hash; parents; message } in
  let commit_data = Commit.encode commit_obj in
  let commit_hash = Store.put db.store commit_data in
  Hashtbl.replace db.branches db.current_branch commit_hash;
  (commit_hash, db)

let checkout db commit_hash =
  let commit_data = Store.get db.store commit_hash in
  let commit_obj = Commit.decode commit_data in
  let state_data = Store.get db.store commit_obj.state in
  let entries = Db_state.decode state_data in
  let tables = List.fold_left (fun acc (e : Db_state.table_entry) ->
    StringMap.add e.name e.root acc
  ) StringMap.empty entries in
  { db with tables }

let parents db commit_hash =
  let commit_data = Store.get db.store commit_hash in
  let commit_obj = Commit.decode commit_data in
  commit_obj.parents
let branch _db ~name:_ = failwith "not implemented"
let switch _db ~name:_ = failwith "not implemented"
let diff _db ~from:_ ~to_:_ ~table:_ = failwith "not implemented"
let merge _db ~ours:_ ~theirs:_ = failwith "not implemented"
