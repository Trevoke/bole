# CLI Executable Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Build a `bole` CLI tool with file-backed persistence, exposing the database API as git-style subcommands.

**Architecture:** Extend `Store` with optional file-backed persistence (`~path`). New `Repo` module bridges `.bole/` directory and `Db.t`. New `bin/main.ml` uses `cmdliner` for subcommand parsing. Each command loads from `.bole/`, runs a `Db` operation, saves back.

**Tech Stack:** OCaml, cmdliner, dune.

**Environment:** Run all commands inside `guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && ...'`. Use `ALCOTEST_COLOR=never` for readable test output.

**Design doc:** `docs/plans/2026-03-14-cli-design.md`

---

### Task 1: File-backed Store

Extend `Store.create` with optional `~path` parameter for file persistence. When `path` is provided, `put` writes to disk alongside Hashtbl, `get` falls back to disk on cache miss, `mem` checks disk on cache miss.

**Files:**
- Modify: `lib/store.mli`
- Modify: `lib/store.ml`
- Create: `test/test_file_store.ml`
- Modify: `test/test_main.ml` (add `Test_file_store.tests`)

**Step 1: Write tests**

Create `test/test_file_store.ml`:

```ocaml
let with_tmp_dir f =
  let dir = Filename.temp_dir "bole-test" "" in
  Fun.protect ~finally:(fun () ->
    (* Clean up *)
    let rec rm path =
      if Sys.is_directory path then begin
        Sys.readdir path |> Array.iter (fun name ->
          rm (Filename.concat path name));
        Sys.rmdir path
      end else
        Sys.remove path
    in
    rm dir
  ) (fun () -> f dir)

let test_file_put_get () =
  with_tmp_dir (fun dir ->
    let store = Bole.Store.create ~path:dir () in
    let data = "hello, file store" in
    let h = Bole.Store.put store data in
    (* Read from a fresh store (empty cache) to verify file persistence *)
    let store2 = Bole.Store.create ~path:dir () in
    let got = Bole.Store.get store2 h in
    Alcotest.(check string) "round-trip via file" data got)

let test_file_mem () =
  with_tmp_dir (fun dir ->
    let store = Bole.Store.create ~path:dir () in
    let h = Bole.Store.put store "data" in
    let store2 = Bole.Store.create ~path:dir () in
    Alcotest.(check bool) "mem from file" true (Bole.Store.mem store2 h);
    let bogus = Bole.Hash.hash "never stored" in
    Alcotest.(check bool) "mem missing" false (Bole.Store.mem store2 bogus))

let test_file_idempotent () =
  with_tmp_dir (fun dir ->
    let store = Bole.Store.create ~path:dir () in
    let h1 = Bole.Store.put store "same" in
    let h2 = Bole.Store.put store "same" in
    let hash_testable =
      Alcotest.testable
        (fun fmt h -> Format.fprintf fmt "%s" (Bole.Hash.to_hex h))
        Bole.Hash.equal
    in
    Alcotest.(check hash_testable) "idempotent" h1 h2)

let test_inmemory_unchanged () =
  (* No path = pure in-memory, same as before *)
  let store = Bole.Store.create () in
  let h = Bole.Store.put store "data" in
  let got = Bole.Store.get store h in
  Alcotest.(check string) "in-memory still works" "data" got

let tests =
  [ "file_store", [
      Alcotest.test_case "file put/get" `Quick test_file_put_get;
      Alcotest.test_case "file mem" `Quick test_file_mem;
      Alcotest.test_case "file idempotent" `Quick test_file_idempotent;
      Alcotest.test_case "in-memory unchanged" `Quick test_inmemory_unchanged;
    ]
  ]
```

**Step 2: Update Store interface**

Modify `lib/store.mli` — change `create`:

```ocaml
(** [create ?path ()] returns a fresh store.
    If [path] is provided, objects are persisted to that directory
    using git-style 2-char prefix subdirectories (e.g. ab/cdef...).
    If [path] is omitted, the store is purely in-memory. *)
val create : ?path:string -> unit -> t
```

**Step 3: Update Store implementation**

Modify `lib/store.ml`:

```ocaml
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
```

**Step 4: Wire up tests, run, commit**

Add `Test_file_store.tests` to `test/test_main.ml`. Run tests — all existing tests pass (they use `Store.create ()` with no `~path`, unchanged behavior), plus 4 new file store tests.

```bash
git add lib/store.mli lib/store.ml test/test_file_store.ml test/test_main.ml
git commit -m "feat: add file-backed persistence to Store"
```

---

### Task 2: Db accessors for Repo

Add accessors to `Db` so the `Repo` module can construct and inspect `Db.t` values. Also add `Hash.of_hex` for reading hex hashes from ref files.

**Files:**
- Modify: `lib/db.mli`
- Modify: `lib/db.ml`
- Modify: `lib/hash.mli`
- Modify: `lib/hash.ml`

**Step 1: Add Db accessors**

Add to `lib/db.mli`:

```ocaml
val current_branch : t -> string
val branch_heads : t -> (string * Hash.t) list
val of_parts :
  store:Store.t ->
  branches:(string * Hash.t) list ->
  current_branch:string ->
  head_commit:Hash.t option ->
  t
```

Implement in `lib/db.ml`:

```ocaml
let current_branch db = db.current_branch

let branch_heads db =
  Hashtbl.fold (fun name hash acc -> (name, hash) :: acc) db.branches []

let of_parts ~store ~branches ~current_branch ~head_commit =
  let branch_tbl = Hashtbl.create 16 in
  List.iter (fun (name, hash) -> Hashtbl.replace branch_tbl name hash) branches;
  let tables = match head_commit with
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
```

**Step 2: Add `Hash.of_hex`**

Add to `lib/hash.mli`:

```ocaml
(** Construct from hex-encoded string. Raises [Invalid_argument] if malformed. *)
val of_hex : string -> t
```

Implement in `lib/hash.ml` — convert hex pairs to bytes, then call `of_raw_string`:

```ocaml
let of_hex hex =
  let len = String.length hex in
  if len <> hash_size * 2 then
    invalid_arg (Printf.sprintf "Hash.of_hex: expected %d hex chars, got %d" (hash_size * 2) len);
  let raw = Bytes.create hash_size in
  for i = 0 to hash_size - 1 do
    let hi = Char.code hex.[i * 2] in
    let lo = Char.code hex.[i * 2 + 1] in
    let hex_val c =
      if c >= 0x30 && c <= 0x39 then c - 0x30
      else if c >= 0x61 && c <= 0x66 then c - 0x61 + 10
      else if c >= 0x41 && c <= 0x46 then c - 0x41 + 10
      else invalid_arg "Hash.of_hex: invalid hex character"
    in
    Bytes.set raw i (Char.chr ((hex_val hi lsl 4) lor hex_val lo))
  done;
  of_raw_string (Bytes.to_string raw)
```

**Step 3: Run tests, commit**

All existing tests should pass unchanged.

```bash
git add lib/db.mli lib/db.ml lib/hash.mli lib/hash.ml
git commit -m "feat: add Db accessors and Hash.of_hex for Repo module"
```

---

### Task 3: Repo module

Create the `Repo` module that bridges `.bole/` directory and `Db.t`.

**Files:**
- Create: `lib/repo.mli`
- Create: `lib/repo.ml`
- Modify: `lib/bole.ml` (add `module Repo = Repo`)
- Create: `test/test_repo.ml`
- Modify: `test/test_main.ml` (add `Test_repo.tests`)

**Step 1: Create the interface**

Create `lib/repo.mli`:

```ocaml
(** Repository: bridge between .bole/ directory and Db.t.

    Handles creating, loading, and saving the on-disk repository
    state including HEAD, branch refs, and the object store. *)

val init : string -> unit
(** [init path] creates a .bole/ directory structure at [path].
    Creates .bole/HEAD, .bole/refs/heads/, .bole/objects/. *)

val load : string -> Db.t
(** [load path] loads a Db.t from the .bole/ directory at [path].
    Reads HEAD for current branch, refs/heads/ for branch pointers,
    and opens the object store. *)

val save : string -> Db.t -> unit
(** [save path db] persists HEAD and branch refs to the .bole/ directory.
    Object store is already persisted via Store.put. *)

val find_root : unit -> string option
(** [find_root ()] searches upward from the current directory for a
    directory containing .bole/. Returns [Some path] or [None]. *)
```

**Step 2: Implement**

Create `lib/repo.ml`:

```ocaml
let bole_dir path = Filename.concat path ".bole"
let head_file path = Filename.concat (bole_dir path) "HEAD"
let refs_dir path = Filename.concat (bole_dir path) "refs"
let heads_dir path = Filename.concat (refs_dir path) "heads"
let objects_dir path = Filename.concat (bole_dir path) "objects"

let mkdir_p dir =
  (* Create directory and parents if needed *)
  let rec go dir =
    if not (Sys.file_exists dir) then begin
      go (Filename.dirname dir);
      Sys.mkdir dir 0o755
    end
  in
  go dir

let write_file path content =
  let oc = open_out path in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () ->
    output_string oc content)

let read_file path =
  let ic = open_in path in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
    let len = in_channel_length ic in
    really_input_string ic len)

let init path =
  mkdir_p (objects_dir path);
  mkdir_p (heads_dir path);
  write_file (head_file path) "main\n"

let load path =
  let head_content = read_file (head_file path) in
  let current_branch = String.trim head_content in
  let store = Store.create ~path:(objects_dir path) () in
  (* Read all branch refs *)
  let branches =
    if Sys.file_exists (heads_dir path) then
      Sys.readdir (heads_dir path)
      |> Array.to_list
      |> List.filter_map (fun name ->
        let ref_file = Filename.concat (heads_dir path) name in
        if Sys.is_directory ref_file then None
        else
          let hex = String.trim (read_file ref_file) in
          Some (name, Hash.of_hex hex))
    else []
  in
  let head_commit =
    List.assoc_opt current_branch branches
  in
  Db.of_parts ~store ~branches ~current_branch ~head_commit

let save path db =
  write_file (head_file path) (Db.current_branch db ^ "\n");
  let heads = heads_dir path in
  mkdir_p heads;
  List.iter (fun (name, hash) ->
    write_file (Filename.concat heads name) (Hash.to_hex hash ^ "\n")
  ) (Db.branch_heads db)

let find_root () =
  let rec search dir =
    let candidate = Filename.concat dir ".bole" in
    if Sys.file_exists candidate && Sys.is_directory candidate then
      Some dir
    else
      let parent = Filename.dirname dir in
      if parent = dir then None  (* reached filesystem root *)
      else search parent
  in
  search (Sys.getcwd ())
```

**Step 3: Write tests**

Create `test/test_repo.ml`:

```ocaml
let with_tmp_dir f =
  let dir = Filename.temp_dir "bole-repo-test" "" in
  Fun.protect ~finally:(fun () ->
    let rec rm path =
      if Sys.is_directory path then begin
        Sys.readdir path |> Array.iter (fun name ->
          rm (Filename.concat path name));
        Sys.rmdir path
      end else
        Sys.remove path
    in
    rm dir
  ) (fun () -> f dir)

let test_init_creates_structure () =
  with_tmp_dir (fun dir ->
    Bole.Repo.init dir;
    Alcotest.(check bool) ".bole exists"
      true (Sys.file_exists (Filename.concat dir ".bole"));
    Alcotest.(check bool) "HEAD exists"
      true (Sys.file_exists (Filename.concat (Filename.concat dir ".bole") "HEAD"));
    Alcotest.(check bool) "objects exists"
      true (Sys.is_directory (Filename.concat (Filename.concat dir ".bole") "objects"));
    Alcotest.(check bool) "refs/heads exists"
      true (Sys.is_directory
        (Filename.concat (Filename.concat (Filename.concat dir ".bole") "refs") "heads")))

let test_round_trip () =
  with_tmp_dir (fun dir ->
    Bole.Repo.init dir;
    let db = Bole.Repo.load dir in
    let db = Bole.Db.put db ~table:"users" ~key:"alice" ~value:"admin" in
    let _h, db = Bole.Db.commit db ~message:"first" in
    Bole.Repo.save dir db;
    (* Load from scratch *)
    let db2 = Bole.Repo.load dir in
    Alcotest.(check (option string)) "persisted value"
      (Some "admin") (Bole.Db.find db2 ~table:"users" ~key:"alice"))

let test_branch_persistence () =
  with_tmp_dir (fun dir ->
    Bole.Repo.init dir;
    let db = Bole.Repo.load dir in
    let db = Bole.Db.put db ~table:"t" ~key:"k" ~value:"v" in
    let _, db = Bole.Db.commit db ~message:"init" in
    let db = Bole.Db.branch db ~name:"feature" in
    let db = Bole.Db.put db ~table:"t" ~key:"k2" ~value:"v2" in
    let _, db = Bole.Db.commit db ~message:"on feature" in
    Bole.Repo.save dir db;
    (* Reload and check branch *)
    let db2 = Bole.Repo.load dir in
    Alcotest.(check string) "on feature branch" "feature"
      (Bole.Db.current_branch db2);
    Alcotest.(check (option string)) "feature has k2"
      (Some "v2") (Bole.Db.find db2 ~table:"t" ~key:"k2"))

let tests =
  [ "repo", [
      Alcotest.test_case "init creates structure" `Quick test_init_creates_structure;
      Alcotest.test_case "round-trip" `Quick test_round_trip;
      Alcotest.test_case "branch persistence" `Quick test_branch_persistence;
    ]
  ]
```

**Step 4: Wire up**

Add `module Repo = Repo` to `lib/bole.ml` (after `module Db = Db`). Add `Test_repo.tests` to `test/test_main.ml`.

**Step 5: Run tests, commit**

```bash
git add lib/repo.mli lib/repo.ml lib/bole.ml test/test_repo.ml test/test_main.ml
git commit -m "feat: add Repo module for .bole/ directory persistence"
```

---

### Task 4: CLI executable with init, put, get

Create the `bin/` directory with `main.ml` using cmdliner. Start with `init`, `put`, and `get` commands — enough to verify the full pipeline works end-to-end.

**Files:**
- Create: `bin/dune`
- Create: `bin/main.ml`
- Modify: `dune-project` (add cmdliner dependency)

**Step 1: Update dune-project**

Add `cmdliner` as a non-test dependency in `dune-project`:

```
(package
 (name bole)
 (synopsis "Content-addressed prolly tree library")
 (depends
  (ocaml (>= 5.2))
  (dune (>= 3.17))
  (digestif (>= 1.2.0))
  (cmdliner (>= 1.3.0))
  (alcotest (and (>= 1.8.0) :with-test))
  (qcheck (and (>= 0.22) :with-test))
  (qcheck-alcotest (and (>= 0.22) :with-test))))
```

**Step 2: Create bin/dune**

```
(executable
 (name main)
 (public_name bole)
 (libraries bole cmdliner))
```

**Step 3: Create bin/main.ml**

```ocaml
open Cmdliner

let find_root_or_die () =
  match Bole.Repo.find_root () with
  | Some path -> path
  | None ->
    Printf.eprintf "fatal: not a bole repository (no .bole/ found)\n";
    exit 1

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
    let db = Bole.Db.put db ~table ~key ~value in
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
    match Bole.Db.find db ~table ~key with
    | Some value -> print_string value; print_newline ()
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
    let db = Bole.Db.delete db ~table ~key in
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
        let commit = Bole.Commit.decode commit_data in
        Printf.printf "%s %s\n" (Bole.Hash.to_hex h) commit.Bole.Commit.message;
        match commit.Bole.Commit.parents with
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
      | Bole.Diff.Added (k, v) -> Printf.printf "+ %s %s\n" k v
      | Bole.Diff.Removed (k, v) -> Printf.printf "- %s %s\n" k v
      | Bole.Diff.Modified (k, old_v, new_v) ->
        Printf.printf "~ %s %s -> %s\n" k old_v new_v)
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
        Printf.printf "  CONFLICT: %s/%s\n" c.table c.key
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
```

**Step 4: Install cmdliner and test build**

Run: `guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && opam install cmdliner -y && dune build' 2>&1`

Note: Check if cmdliner is already installed via `opam list cmdliner`. It may already be present as a transitive dependency of alcotest.

**Step 5: Test manually**

```bash
cd /tmp && mkdir bole-test && cd bole-test
bole init
bole put users alice admin
bole get users alice     # should print "admin"
bole commit -m "first"   # should print hash
bole log                  # should print hash + message
```

**Step 6: Run library tests to verify nothing broke**

Run: `guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && ALCOTEST_COLOR=never dune test'`

**Step 7: Commit**

```bash
git add bin/dune bin/main.ml dune-project
git commit -m "feat: add bole CLI with init, put, get, delete, commit, log, branch, switch, diff, merge"
```

---

### Task 5: End-to-end CLI test

Write a shell script or test that exercises the full CLI workflow to verify everything works together.

**Files:**
- Create: `test/test_cli.sh`

**Step 1: Write the test script**

Create `test/test_cli.sh`:

```bash
#!/bin/bash
set -euo pipefail

BOLE="dune exec bin/main.exe --"
DIR=$(mktemp -d)
cd "$DIR"

echo "=== init ==="
$BOLE init

echo "=== put/get ==="
$BOLE put users alice admin
$BOLE put users bob editor
test "$($BOLE get users alice)" = "admin"
test "$($BOLE get users bob)" = "editor"

echo "=== commit ==="
C1=$($BOLE commit -m "initial")
echo "commit1: $C1"

echo "=== modify and commit ==="
$BOLE put users alice superadmin
C2=$($BOLE commit -m "promote alice")

echo "=== log ==="
$BOLE log

echo "=== diff ==="
$BOLE diff "$C1" "$C2" users

echo "=== branch and merge ==="
$BOLE switch main
$BOLE branch feature
$BOLE put users carol new
C3=$($BOLE commit -m "add carol on feature")
$BOLE switch main
$BOLE merge feature
test "$($BOLE get users carol)" = "new"
test "$($BOLE get users alice)" = "superadmin"

echo "=== cleanup ==="
rm -rf "$DIR"
echo "ALL TESTS PASSED"
```

**Step 2: Run the test**

Run: `guix shell -m manifest.scm -- bash -c 'unset OCAMLPATH && eval $(opam env) && bash test/test_cli.sh'`

**Step 3: Commit**

```bash
git add test/test_cli.sh
git commit -m "test: add end-to-end CLI test script"
```
