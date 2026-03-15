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
  ) t)

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

(* --- put --- *)

let put_cmd =
  let run table key value =
    let path = find_root_or_die () in
    let db = Bole.Repo.load path in
    let db = Bole.Db.put db ~table
      ~key:[Bole.Tuple.String key]
      ~value:[Bole.Tuple.String value] in
    Bole.Repo.save path db
  in
  let table = Arg.(required & pos 0 (some string) None & info [] ~docv:"TABLE") in
  let key = Arg.(required & pos 1 (some string) None & info [] ~docv:"KEY") in
  let value = Arg.(required & pos 2 (some string) None & info [] ~docv:"VALUE") in
  let doc = "Insert or update a key-value pair in a table" in
  let info = Cmd.info "put" ~doc in
  Cmd.v info Term.(const run $ table $ key $ value)

(* --- get --- *)

let get_cmd =
  let run table key =
    let path = find_root_or_die () in
    let db = Bole.Repo.load path in
    match Bole.Db.find db ~table ~key:[Bole.Tuple.String key] with
    | Some value -> print_string (render_tuple value); print_newline ()
    | None ->
      Printf.eprintf "not found: %s/%s\n" table key;
      exit 1
  in
  let table = Arg.(required & pos 0 (some string) None & info [] ~docv:"TABLE") in
  let key = Arg.(required & pos 1 (some string) None & info [] ~docv:"KEY") in
  let doc = "Look up a key in a table" in
  let info = Cmd.info "get" ~doc in
  Cmd.v info Term.(const run $ table $ key)

(* --- delete --- *)

let delete_cmd =
  let run table key =
    let path = find_root_or_die () in
    let db = Bole.Repo.load path in
    let db = Bole.Db.delete db ~table ~key:[Bole.Tuple.String key] in
    Bole.Repo.save path db
  in
  let table = Arg.(required & pos 0 (some string) None & info [] ~docv:"TABLE") in
  let key = Arg.(required & pos 1 (some string) None & info [] ~docv:"KEY") in
  let doc = "Delete a key from a table" in
  let info = Cmd.info "delete" ~doc in
  Cmd.v info Term.(const run $ table $ key)

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
    init_cmd; put_cmd; get_cmd; delete_cmd;
    commit_cmd; log_cmd;
    branch_cmd; switch_cmd;
    diff_cmd; merge_cmd;
  ] in
  exit (Cmd.eval cmd)
