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
  base : string option;
  ours : string option;
  theirs : string option;
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

let current_branch db = db.current_branch

let branch_heads db =
  Hashtbl.fold (fun name hash acc -> (name, hash) :: acc) db.branches []

let working_tables db =
  StringMap.fold (fun name root acc -> (name, root) :: acc) db.tables []

let of_parts ~store ~branches ~current_branch ~head_commit ?(working_tables=[]) () =
  let branch_tbl = Hashtbl.create 16 in
  List.iter (fun (name, hash) -> Hashtbl.replace branch_tbl name hash) branches;
  let tables = match working_tables with
    | _ :: _ ->
      List.fold_left (fun acc (name, root) ->
        StringMap.add name root acc
      ) StringMap.empty working_tables
    | [] ->
      match head_commit with
      | Some h ->
        let commit_data = Store.get store h in
        let commit_obj = Commit.decode commit_data in
        let state_data = Store.get store commit_obj.state in
        let entries = Db_state.decode state_data in
        List.fold_left (fun acc (e : Db_state.table_entry) ->
          StringMap.add e.name e.root acc
        ) StringMap.empty entries
      | None -> StringMap.empty
  in
  { store; branches = branch_tbl; current_branch; tables }

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

let range db ~table =
  match StringMap.find_opt table db.tables with
  | None -> Seq.empty
  | Some root -> Tree.range db.store root

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
let branch db ~name =
  let head = Hashtbl.find db.branches db.current_branch in
  Hashtbl.replace db.branches name head;
  { db with current_branch = name }

let switch db ~name =
  let commit_hash = Hashtbl.find db.branches name in
  let db = checkout db commit_hash in
  { db with current_branch = name }
let table_root_from_commit db commit_hash table =
  let commit_data = Store.get db.store commit_hash in
  let commit_obj = Commit.decode commit_data in
  let state_data = Store.get db.store commit_obj.state in
  let entries = Db_state.decode state_data in
  match List.find_opt (fun (e : Db_state.table_entry) -> e.name = table) entries with
  | Some e -> e.root
  | None -> empty_tree_root db.store

let diff db ~from ~to_ ~table =
  let from_root = table_root_from_commit db from table in
  let to_root = table_root_from_commit db to_ table in
  Diff.diff db.store ~from:from_root ~to_:to_root
let load_tables db commit_hash =
  let commit_data = Store.get db.store commit_hash in
  let commit_obj = Commit.decode commit_data in
  let state_data = Store.get db.store commit_obj.state in
  let entries = Db_state.decode state_data in
  List.fold_left (fun acc (e : Db_state.table_entry) ->
    StringMap.add e.name e.root acc
  ) StringMap.empty entries

let find_ancestor db h1 h2 =
  if Hash.equal h1 h2 then h1
  else begin
    let module HashSet = Set.Make(struct
      type t = Hash.t
      let compare = Hash.compare
    end) in
    let seen1 = ref (HashSet.singleton h1) in
    let seen2 = ref (HashSet.singleton h2) in
    let queue1 = Queue.create () in
    let queue2 = Queue.create () in
    Queue.push h1 queue1;
    Queue.push h2 queue2;
    let found = ref None in
    while !found = None && (not (Queue.is_empty queue1) || not (Queue.is_empty queue2)) do
      if not (Queue.is_empty queue1) then begin
        let h = Queue.pop queue1 in
        let ps = parents db h in
        List.iter (fun p ->
          if HashSet.mem p !seen2 then found := Some p
          else if not (HashSet.mem p !seen1) then begin
            seen1 := HashSet.add p !seen1;
            Queue.push p queue1
          end
        ) ps
      end;
      if !found = None && not (Queue.is_empty queue2) then begin
        let h = Queue.pop queue2 in
        let ps = parents db h in
        List.iter (fun p ->
          if HashSet.mem p !seen1 then found := Some p
          else if not (HashSet.mem p !seen2) then begin
            seen2 := HashSet.add p !seen2;
            Queue.push p queue2
          end
        ) ps
      end
    done;
    match !found with
    | Some h -> h
    | None -> raise Not_found
  end

let merge db ~ours ~theirs =
  let ours_hash = Hashtbl.find db.branches ours in
  let theirs_hash = Hashtbl.find db.branches theirs in
  let ancestor_hash = find_ancestor db ours_hash theirs_hash in
  let ancestor_tables = load_tables db ancestor_hash in
  let ours_tables = load_tables db ours_hash in
  let theirs_tables = load_tables db theirs_hash in
  let empty_root = empty_tree_root db.store in
  (* Collect union of all table names *)
  let all_names =
    StringMap.union (fun _ a _ -> Some a) ancestor_tables ours_tables
    |> StringMap.union (fun _ a _ -> Some a) theirs_tables
  in
  let merged_tables = ref StringMap.empty in
  let all_conflicts = ref [] in
  StringMap.iter (fun name _ ->
    let a_root = match StringMap.find_opt name ancestor_tables with
      | Some r -> r | None -> empty_root in
    let o_root = match StringMap.find_opt name ours_tables with
      | Some r -> r | None -> empty_root in
    let t_root = match StringMap.find_opt name theirs_tables with
      | Some r -> r | None -> empty_root in
    if Hash.equal o_root t_root then
      merged_tables := StringMap.add name o_root !merged_tables
    else if Hash.equal o_root a_root then
      merged_tables := StringMap.add name t_root !merged_tables
    else if Hash.equal t_root a_root then
      merged_tables := StringMap.add name o_root !merged_tables
    else begin
      let ours_diff = Diff.diff db.store ~from:a_root ~to_:o_root in
      let theirs_diff = Diff.diff db.store ~from:a_root ~to_:t_root in
      let result = Merge.three_way ~ours:ours_diff ~theirs:theirs_diff in
      let final_root = List.fold_left (fun root change ->
        match change with
        | Merge.Put (k, v) -> Tree.put db.store root k v
        | Merge.Delete k -> Tree.delete db.store root k
      ) o_root result.Merge.changes in
      merged_tables := StringMap.add name final_root !merged_tables;
      List.iter (fun (c : Merge.conflict) ->
        all_conflicts := {
          table = name;
          key = c.key;
          base = c.base;
          ours = c.ours;
          theirs = c.theirs;
        } :: !all_conflicts
      ) result.Merge.conflicts
    end
  ) all_names;
  { db = { db with tables = !merged_tables };
    conflicts = List.rev !all_conflicts }
