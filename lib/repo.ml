let bole_dir path = Filename.concat path ".bole"
let head_file path = Filename.concat (bole_dir path) "HEAD"
let refs_dir path = Filename.concat (bole_dir path) "refs"
let heads_dir path = Filename.concat (refs_dir path) "heads"
let objects_dir path = Filename.concat (bole_dir path) "objects"

let mkdir_p dir =
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
      if parent = dir then None
      else search parent
  in
  search (Sys.getcwd ())
