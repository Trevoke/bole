module StringMap = Map.Make(String)

type table_state = { root : Hash.t; schema : Hash.t }

type t = {
  store : Store.t;
  branches : (string, Hash.t) Hashtbl.t; [@warning "-69"]
  current_branch : string; [@warning "-69"]
  tables : table_state StringMap.t;
} [@@warning "-69"]

type diff_entry =
  | Added of Tuple.t * Tuple.t
  | Removed of Tuple.t * Tuple.t
  | Modified of Tuple.t * Tuple.t * Tuple.t

type conflict = {
  table : string;
  key : Tuple.t;
  base : Tuple.t option;
  ours : Tuple.t option;
  theirs : Tuple.t option;
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

let working_state db =
  StringMap.fold (fun name ts acc -> (name, ts.root, ts.schema) :: acc) db.tables []

let load_schema store schema_hash =
  Schema.decode (Store.get store schema_hash)

let split_row schema row =
  let key_cols = Schema.key_columns schema in
  let val_cols = Schema.value_columns schema in
  let key = List.map (fun (name, _) -> List.assoc name row) key_cols in
  let value = List.map (fun (name, _) -> List.assoc name row) val_cols in
  (key, value)

let merge_row schema key_tuple value_tuple =
  let key_cols = Schema.key_columns schema in
  let val_cols = Schema.value_columns schema in
  let key_pairs = List.combine (List.map fst key_cols) key_tuple in
  let val_pairs = List.combine (List.map fst val_cols) value_tuple in
  key_pairs @ val_pairs

let load_tables_from_commit store commit_hash =
  let commit_data = Store.get store commit_hash in
  let commit_obj = Commit.decode commit_data in
  let state_data = Store.get store commit_obj.state in
  let entries = Db_state.decode state_data in
  List.fold_left (fun acc (e : Db_state.table_entry) ->
    StringMap.add e.name { root = e.root; schema = e.schema } acc
  ) StringMap.empty entries

let of_parts ~store ~branches ~current_branch ~head_commit ?(working_state=[]) () =
  let branch_tbl = Hashtbl.create 16 in
  List.iter (fun (name, hash) -> Hashtbl.replace branch_tbl name hash) branches;
  let tables = match working_state with
    | _ :: _ ->
      List.fold_left (fun acc (name, root, schema) ->
        StringMap.add name { root; schema } acc
      ) StringMap.empty working_state
    | [] ->
      match head_commit with
      | Some h -> load_tables_from_commit store h
      | None -> StringMap.empty
  in
  { store; branches = branch_tbl; current_branch; tables }

let create_table db ~table ~schema =
  let schema_data = Schema.encode schema in
  let schema_hash = Store.put db.store schema_data in
  let root = empty_tree_root db.store in
  { db with tables = StringMap.add table { root; schema = schema_hash } db.tables }

let put_row db ~table ~row =
  let ts = StringMap.find table db.tables in
  let schema = load_schema db.store ts.schema in
  let key_tuple, val_tuple = split_row schema row in
  let key_bytes = Tuple.encode key_tuple in
  let val_bytes = Tuple.encode val_tuple in
  let root' = Tree.put db.store ts.root key_bytes val_bytes in
  { db with tables = StringMap.add table { ts with root = root' } db.tables }

let get_row db ~table ~key =
  match StringMap.find_opt table db.tables with
  | None -> None
  | Some ts ->
    let key_bytes = Tuple.encode key in
    match Tree.find db.store ts.root key_bytes with
    | None -> None
    | Some v ->
      let schema = load_schema db.store ts.schema in
      let val_tuple = Tuple.decode v in
      Some (merge_row schema key val_tuple)

let delete_row db ~table ~key =
  let ts = StringMap.find table db.tables in
  let key_bytes = Tuple.encode key in
  let root' = Tree.delete db.store ts.root key_bytes in
  { db with tables = StringMap.add table { ts with root = root' } db.tables }

let range_rows db ~table =
  match StringMap.find_opt table db.tables with
  | None -> Seq.empty
  | Some ts ->
    let schema = load_schema db.store ts.schema in
    Tree.range db.store ts.root
    |> Seq.map (fun (k, v) ->
      merge_row schema (Tuple.decode k) (Tuple.decode v))

let commit db ~message =
  let entries = StringMap.fold (fun name (ts : table_state) acc ->
    Db_state.{ name; root = ts.root; schema = ts.schema } :: acc
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
  let tables = load_tables_from_commit db.store commit_hash in
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
  |> Seq.map (fun entry ->
    match entry with
    | Diff.Added (k, v) -> Added (Tuple.decode k, Tuple.decode v)
    | Diff.Removed (k, v) -> Removed (Tuple.decode k, Tuple.decode v)
    | Diff.Modified (k, o, n) ->
      Modified (Tuple.decode k, Tuple.decode o, Tuple.decode n))

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
  let ancestor_tables = load_tables_from_commit db.store ancestor_hash in
  let ours_tables = load_tables_from_commit db.store ours_hash in
  let theirs_tables = load_tables_from_commit db.store theirs_hash in
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
      | Some ts -> ts.root | None -> empty_root in
    let o_root = match StringMap.find_opt name ours_tables with
      | Some ts -> ts.root | None -> empty_root in
    let t_root = match StringMap.find_opt name theirs_tables with
      | Some ts -> ts.root | None -> empty_root in
    if Hash.equal o_root t_root then
      merged_tables := StringMap.add name (StringMap.find name ours_tables) !merged_tables
    else if Hash.equal o_root a_root then
      merged_tables := StringMap.add name (StringMap.find name theirs_tables) !merged_tables
    else if Hash.equal t_root a_root then
      merged_tables := StringMap.add name (StringMap.find name ours_tables) !merged_tables
    else begin
      let ours_diff = Diff.diff db.store ~from:a_root ~to_:o_root in
      let theirs_diff = Diff.diff db.store ~from:a_root ~to_:t_root in
      let result = Merge.three_way ~ours:ours_diff ~theirs:theirs_diff in

      (* Load schema for cell-level merge *)
      let ts = StringMap.find name ours_tables in
      let schema = load_schema db.store ts.schema in
      let val_cols = Schema.value_columns schema in
      let n_fields = List.length val_cols in

      (* Apply non-conflicting changes first *)
      let final_root = List.fold_left (fun root change ->
        match change with
        | Merge.Put (k, v) -> Tree.put db.store root k v
        | Merge.Delete k -> Tree.delete db.store root k
      ) o_root result.Merge.changes in

      (* Try cell-level resolution for each conflict *)
      let final_root = ref final_root in
      List.iter (fun (c : Merge.conflict) ->
        match c.base with
        | None ->
          (* Both added — can't do cell-level *)
          all_conflicts := {
            table = name;
            key = Tuple.decode c.key;
            base = Option.map Tuple.decode c.base;
            ours = Option.map Tuple.decode c.ours;
            theirs = Option.map Tuple.decode c.theirs;
          } :: !all_conflicts
        | Some base_bytes ->
          let base_vals = Tuple.decode base_bytes in
          let ours_vals = match c.ours with
            | Some b -> Tuple.decode b | None -> base_vals in
          let theirs_vals = match c.theirs with
            | Some b -> Tuple.decode b | None -> base_vals in
          if n_fields = 0 || List.length base_vals <> n_fields then
            (* Schema mismatch or no value columns — fall back to whole-value conflict *)
            all_conflicts := {
              table = name;
              key = Tuple.decode c.key;
              base = Some base_vals;
              ours = Some ours_vals;
              theirs = Some theirs_vals;
            } :: !all_conflicts
          else begin
            let has_conflict = ref false in
            let merged = List.init n_fields (fun i ->
              let b = List.nth base_vals i in
              let o = List.nth ours_vals i in
              let t = List.nth theirs_vals i in
              if o = b then t
              else if t = b then o
              else if o = t then o
              else begin has_conflict := true; o end
            ) in
            if !has_conflict then
              all_conflicts := {
                table = name;
                key = Tuple.decode c.key;
                base = Some base_vals;
                ours = Some ours_vals;
                theirs = Some theirs_vals;
              } :: !all_conflicts
            else begin
              let merged_bytes = Tuple.encode merged in
              final_root := Tree.put db.store !final_root c.key merged_bytes
            end
          end
      ) result.Merge.conflicts;
      merged_tables := StringMap.add name { root = !final_root; schema = ts.schema } !merged_tables
    end
  ) all_names;
  { db = { db with tables = !merged_tables };
    conflicts = List.rev !all_conflicts }
