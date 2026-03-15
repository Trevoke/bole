open Cmdliner

let find_root_or_die () =
  match Bole.Repo.find_root () with
  | Some path -> path
  | None ->
    Printf.eprintf "fatal: not a bole repository (no .bole/ found)\n";
    exit 1

let render_tuple t =
  String.concat " " (List.map (function
    | Bole.Tuple.String s -> s
    | Bole.Tuple.Int64 n -> Int64.to_string n
    | Bole.Tuple.Uuid u -> Bole.Uuid.to_hex u
    | Bole.Tuple.Bool b -> string_of_bool b
    | Bole.Tuple.Float f -> Float.to_string f
    | Bole.Tuple.Timestamp ts -> Int64.to_string ts
    | Bole.Tuple.Blob b -> Printf.sprintf "<blob:%d>" (String.length b)
  ) t)

let render_row row =
  String.concat " " (List.map (fun (col, v) ->
    Printf.sprintf "%s=%s" col (match v with
      | Bole.Tuple.String s -> s
      | Bole.Tuple.Int64 n -> Int64.to_string n
      | Bole.Tuple.Uuid u -> Bole.Uuid.to_hex u
      | Bole.Tuple.Bool b -> string_of_bool b
      | Bole.Tuple.Float f -> Float.to_string f
      | Bole.Tuple.Timestamp ts -> Int64.to_string ts
      | Bole.Tuple.Blob b -> Printf.sprintf "<blob:%d>" (String.length b))
  ) row)

let parse_assignments assignments =
  List.map (fun s ->
    match String.index_opt s '=' with
    | Some i ->
      let col = String.sub s 0 i in
      let value = String.sub s (i + 1) (String.length s - i - 1) in
      let v = match Int64.of_string_opt value with
        | Some n -> Bole.Tuple.Int64 n
        | None -> Bole.Tuple.String value
      in
      (col, v)
    | None ->
      Printf.eprintf "invalid assignment: %s (expected col=value)\n" s;
      exit 1
  ) assignments

(* --- init --- *)

let init_cmd =
  let run () =
    let path = Sys.getcwd () in
    if Sys.file_exists (Filename.concat path ".bole") then begin
      Printf.eprintf "error: .bole/ already exists\n";
      exit 1
    end;
    Bole.Repo.init path;
    Printf.printf "Initialized empty bole repository in %s/.bole/\n" path
  in
  let doc = "Create a new bole repository" in
  let info = Cmd.info "init" ~doc in
  Cmd.v info Term.(const run $ const ())

(* --- create-table --- *)

let create_table_cmd =
  let run table columns =
    let path = find_root_or_die () in
    let db = Bole.Repo.load path in
    let parse_col s =
      match String.split_on_char ':' s with
      | [name; "int64"] -> (name, Bole.Schema.Int64)
      | [name; "string"] -> (name, Bole.Schema.Str)
      | _ ->
        Printf.eprintf "invalid column spec: %s (expected name:type, type is int64 or string)\n" s;
        exit 1
    in
    let cols = List.map parse_col columns in
    let schema = Bole.Schema.create ~columns:cols in
    let db = Bole.Db.create_table db ~table ~schema in
    Bole.Repo.save path db;
    Printf.printf "Created table '%s'\n" table
  in
  let table = Arg.(required & pos 0 (some string) None & info [] ~docv:"TABLE") in
  let columns = Arg.(non_empty & pos_right 0 string [] & info [] ~docv:"COL:TYPE") in
  let doc = "Create a table with a schema" in
  let info = Cmd.info "create-table" ~doc in
  Cmd.v info Term.(const run $ table $ columns)

(* --- put --- *)

let put_cmd =
  let run table assignments =
    let path = find_root_or_die () in
    let db = Bole.Repo.load path in
    let row = parse_assignments assignments in
    let uuid, db = Bole.Db.put_row db ~table ~row in
    Bole.Repo.save path db;
    Printf.printf "%s\n" (Bole.Uuid.to_hex uuid)
  in
  let table = Arg.(required & pos 0 (some string) None & info [] ~docv:"TABLE") in
  let assignments = Arg.(non_empty & pos_right 0 string [] & info [] ~docv:"COL=VALUE") in
  let doc = "Insert a row in a table" in
  let info = Cmd.info "put" ~doc in
  Cmd.v info Term.(const run $ table $ assignments)

(* --- get --- *)

let get_cmd =
  let run table id_str =
    let path = find_root_or_die () in
    let db = Bole.Repo.load path in
    let id = Bole.Uuid.of_string id_str in
    match Bole.Db.get_row db ~table ~id with
    | Some row ->
      print_string (render_row row);
      print_newline ()
    | None ->
      Printf.eprintf "not found\n";
      exit 1
  in
  let table = Arg.(required & pos 0 (some string) None & info [] ~docv:"TABLE") in
  let id_str = Arg.(required & pos 1 (some string) None & info [] ~docv:"UUID") in
  let doc = "Look up a row by UUID" in
  let info = Cmd.info "get" ~doc in
  Cmd.v info Term.(const run $ table $ id_str)

(* --- update --- *)

let update_cmd =
  let run table id_str assignments =
    let path = find_root_or_die () in
    let db = Bole.Repo.load path in
    let id = Bole.Uuid.of_string id_str in
    let row = parse_assignments assignments in
    let db = Bole.Db.update_row db ~table ~id ~row in
    Bole.Repo.save path db
  in
  let table = Arg.(required & pos 0 (some string) None & info [] ~docv:"TABLE") in
  let id_str = Arg.(required & pos 1 (some string) None & info [] ~docv:"UUID") in
  let assignments = Arg.(non_empty & pos_right 1 string [] & info [] ~docv:"COL=VALUE") in
  let doc = "Update a row by UUID" in
  let info = Cmd.info "update" ~doc in
  Cmd.v info Term.(const run $ table $ id_str $ assignments)

(* --- delete --- *)

let delete_cmd =
  let run table id_str =
    let path = find_root_or_die () in
    let db = Bole.Repo.load path in
    let id = Bole.Uuid.of_string id_str in
    let db = Bole.Db.delete_row db ~table ~id in
    Bole.Repo.save path db
  in
  let table = Arg.(required & pos 0 (some string) None & info [] ~docv:"TABLE") in
  let id_str = Arg.(required & pos 1 (some string) None & info [] ~docv:"UUID") in
  let doc = "Delete a row by UUID" in
  let info = Cmd.info "delete" ~doc in
  Cmd.v info Term.(const run $ table $ id_str)

(* --- commit --- *)

let commit_cmd =
  let run message =
    let path = find_root_or_die () in
    let db = Bole.Repo.load path in
    let hash, db = Bole.Db.commit db ~message in
    Bole.Repo.save path db;
    Printf.printf "%s\n" (Bole.Hash.to_hex hash)
  in
  let message = Arg.(required & opt (some string) None & info ["m"; "message"] ~docv:"MSG" ~doc:"Commit message") in
  let doc = "Snapshot current database state" in
  let info = Cmd.info "commit" ~doc in
  Cmd.v info Term.(const run $ message)

(* --- log --- *)

let log_cmd =
  let run () =
    let path = find_root_or_die () in
    let db = Bole.Repo.load path in
    let branch = Bole.Db.current_branch db in
    let heads = Bole.Db.branch_heads db in
    match List.assoc_opt branch heads with
    | None -> Printf.printf "(no commits)\n"
    | Some head ->
      let rec walk h =
        let commit_data = Bole.Store.get (Bole.Db.store db) h in
        let commit_obj = Bole.Commit.decode commit_data in
        Printf.printf "%s %s\n" (Bole.Hash.to_hex h) commit_obj.Bole.Commit.message;
        match commit_obj.Bole.Commit.parents with
        | [] -> ()
        | parent :: _ -> walk parent
      in
      walk head
  in
  let doc = "Show commit history" in
  let info = Cmd.info "log" ~doc in
  Cmd.v info Term.(const run $ const ())

(* --- branch --- *)

let branch_cmd =
  let run name =
    let path = find_root_or_die () in
    let db = Bole.Repo.load path in
    let db = Bole.Db.branch db ~name in
    Bole.Repo.save path db;
    Printf.printf "Switched to new branch '%s'\n" name
  in
  let name = Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME") in
  let doc = "Create and switch to a new branch" in
  let info = Cmd.info "branch" ~doc in
  Cmd.v info Term.(const run $ name)

(* --- switch --- *)

let switch_cmd =
  let run name =
    let path = find_root_or_die () in
    let db = Bole.Repo.load path in
    let db = Bole.Db.switch db ~name in
    Bole.Repo.save path db;
    Printf.printf "Switched to branch '%s'\n" name
  in
  let name = Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME") in
  let doc = "Switch to an existing branch" in
  let info = Cmd.info "switch" ~doc in
  Cmd.v info Term.(const run $ name)

(* --- diff --- *)

let diff_cmd =
  let run commit1 commit2 table =
    let path = find_root_or_die () in
    let db = Bole.Repo.load path in
    let h1 = Bole.Hash.of_hex commit1 in
    let h2 = Bole.Hash.of_hex commit2 in
    Bole.Db.diff db ~from:h1 ~to_:h2 ~table
    |> Seq.iter (fun entry ->
      match entry with
      | Bole.Db.Added (k, v) ->
        Printf.printf "+ %s %s\n" (render_tuple k) (render_tuple v)
      | Bole.Db.Removed (k, v) ->
        Printf.printf "- %s %s\n" (render_tuple k) (render_tuple v)
      | Bole.Db.Modified (k, old_v, new_v) ->
        Printf.printf "~ %s %s -> %s\n" (render_tuple k)
          (render_tuple old_v) (render_tuple new_v))
  in
  let commit1 = Arg.(required & pos 0 (some string) None & info [] ~docv:"COMMIT1") in
  let commit2 = Arg.(required & pos 1 (some string) None & info [] ~docv:"COMMIT2") in
  let table = Arg.(required & pos 2 (some string) None & info [] ~docv:"TABLE") in
  let doc = "Show differences between two commits for a table" in
  let info = Cmd.info "diff" ~doc in
  Cmd.v info Term.(const run $ commit1 $ commit2 $ table)

(* --- merge --- *)

let merge_cmd =
  let run branch_name =
    let path = find_root_or_die () in
    let db = Bole.Repo.load path in
    let current = Bole.Db.current_branch db in
    let result = Bole.Db.merge db ~ours:current ~theirs:branch_name in
    Bole.Repo.save path result.Bole.Db.db;
    if result.Bole.Db.conflicts = [] then
      Printf.printf "Merge successful\n"
    else begin
      Printf.printf "Merge completed with %d conflict(s):\n"
        (List.length result.Bole.Db.conflicts);
      List.iter (fun (c : Bole.Db.conflict) ->
        Printf.printf "  CONFLICT: %s/%s\n" c.table (render_tuple c.key)
      ) result.Bole.Db.conflicts
    end
  in
  let branch_name = Arg.(required & pos 0 (some string) None & info [] ~docv:"BRANCH") in
  let doc = "Merge a branch into the current branch" in
  let info = Cmd.info "merge" ~doc in
  Cmd.v info Term.(const run $ branch_name)

(* --- main --- *)

let () =
  let doc = "A diffable, mergeable database" in
  let info = Cmd.info "bole" ~version:"0.1.0" ~doc in
  let cmd = Cmd.group info [
    init_cmd; create_table_cmd; put_cmd; update_cmd; get_cmd; delete_cmd;
    commit_cmd; log_cmd;
    branch_cmd; switch_cmd;
    diff_cmd; merge_cmd;
  ] in
  exit (Cmd.eval cmd)
