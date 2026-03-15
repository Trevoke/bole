(* Acceptance tests for bole as a diffable, mergeable database.
   These tests define the user-facing behavior we're building toward.
   They will fail until the Db module and supporting layers are implemented. *)

open Bole.Tuple

let tuple_testable = Alcotest.testable
  (fun fmt t -> Format.fprintf fmt "[%s]"
    (String.concat "; " (List.map (function
      | String s -> Printf.sprintf "String %S" s
      | Int64 n -> Printf.sprintf "Int64 %Ld" n
    ) t)))
  (=)

let opt_tuple = Alcotest.option tuple_testable

let tuple_value = Alcotest.testable
  (fun fmt v -> match v with
    | String s -> Format.fprintf fmt "String %S" s
    | Int64 n -> Format.fprintf fmt "Int64 %Ld" n)
  (=)

let simple_schema = Bole.Schema.create
  ~columns:["key", Bole.Schema.Str; "value", Bole.Schema.Str]
  ~primary_key:["key"]

(* --- Test 1: Basic table operations --- *)

let test_basic_table_workflow () =
  let db = Bole.Db.create () in
  let db = Bole.Db.create_table db ~table:"users" ~schema:simple_schema in
  let db = Bole.Db.put_row db ~table:"users" ~row:["key", String "alice"; "value", String "admin"] in
  let db = Bole.Db.put_row db ~table:"users" ~row:["key", String "bob"; "value", String "editor"] in
  (match Bole.Db.get_row db ~table:"users" ~key:[String "alice"] with
   | Some row -> Alcotest.(check tuple_value) "find alice" (String "admin") (List.assoc "value" row)
   | None -> Alcotest.fail "alice not found");
  (match Bole.Db.get_row db ~table:"users" ~key:[String "bob"] with
   | Some row -> Alcotest.(check tuple_value) "find bob" (String "editor") (List.assoc "value" row)
   | None -> Alcotest.fail "bob not found");
  Alcotest.(check bool) "different table is independent"
    true (Option.is_none (Bole.Db.get_row db ~table:"posts" ~key:[String "alice"]))

(* --- Test 2: Commit and history --- *)

let test_commit_and_history () =
  let db = Bole.Db.create () in
  let db = Bole.Db.create_table db ~table:"users" ~schema:simple_schema in
  let db = Bole.Db.put_row db ~table:"users" ~row:["key", String "alice"; "value", String "v1"] in
  let commit1, db = Bole.Db.commit db ~message:"first" in
  let db = Bole.Db.put_row db ~table:"users" ~row:["key", String "alice"; "value", String "v2"] in
  let commit2, _db = Bole.Db.commit db ~message:"second" in
  (match Bole.Db.get_row db ~table:"users" ~key:[String "alice"] with
   | Some row -> Alcotest.(check tuple_value) "current state sees v2" (String "v2") (List.assoc "value" row)
   | None -> Alcotest.fail "alice not found");
  let old = Bole.Db.checkout db commit1 in
  (match Bole.Db.get_row old ~table:"users" ~key:[String "alice"] with
   | Some row -> Alcotest.(check tuple_value) "old commit sees v1" (String "v1") (List.assoc "value" row)
   | None -> Alcotest.fail "alice not found in old");
  let parents = Bole.Db.parents db commit2 in
  let hash_testable =
    Alcotest.testable
      (fun fmt h -> Format.fprintf fmt "%s" (Bole.Hash.to_hex h))
      Bole.Hash.equal
  in
  Alcotest.(check (list hash_testable)) "commit2 parent is commit1"
    [commit1] parents

(* --- Test 3: Branching --- *)

let test_branching () =
  let db = Bole.Db.create () in
  let db = Bole.Db.create_table db ~table:"users" ~schema:simple_schema in
  let db = Bole.Db.put_row db ~table:"users" ~row:["key", String "alice"; "value", String "v1"] in
  let _c1, db = Bole.Db.commit db ~message:"initial" in
  let db = Bole.Db.branch db ~name:"feature" in
  let db = Bole.Db.put_row db ~table:"users" ~row:["key", String "bob"; "value", String "new"] in
  let _c2, db = Bole.Db.commit db ~message:"add bob on feature" in
  let db = Bole.Db.switch db ~name:"main" in
  Alcotest.(check bool) "main doesn't see bob"
    true (Option.is_none (Bole.Db.get_row db ~table:"users" ~key:[String "bob"]));
  (match Bole.Db.get_row db ~table:"users" ~key:[String "alice"] with
   | Some row -> Alcotest.(check tuple_value) "main sees alice" (String "v1") (List.assoc "value" row)
   | None -> Alcotest.fail "alice not found on main");
  let db = Bole.Db.switch db ~name:"feature" in
  (match Bole.Db.get_row db ~table:"users" ~key:[String "bob"] with
   | Some row -> Alcotest.(check tuple_value) "feature sees bob" (String "new") (List.assoc "value" row)
   | None -> Alcotest.fail "bob not found on feature");
  (match Bole.Db.get_row db ~table:"users" ~key:[String "alice"] with
   | Some row -> Alcotest.(check tuple_value) "feature sees alice" (String "v1") (List.assoc "value" row)
   | None -> Alcotest.fail "alice not found on feature")

(* --- Test 4: Diff between commits --- *)

let test_diff_branches () =
  let db = Bole.Db.create () in
  let db = Bole.Db.create_table db ~table:"users" ~schema:simple_schema in
  let db = Bole.Db.put_row db ~table:"users" ~row:["key", String "alice"; "value", String "v1"] in
  let db = Bole.Db.put_row db ~table:"users" ~row:["key", String "bob"; "value", String "v1"] in
  let c1, db = Bole.Db.commit db ~message:"initial" in
  let db = Bole.Db.put_row db ~table:"users" ~row:["key", String "alice"; "value", String "v2"] in
  let db = Bole.Db.delete_row db ~table:"users" ~key:[String "bob"] in
  let db = Bole.Db.put_row db ~table:"users" ~row:["key", String "carol"; "value", String "new"] in
  let c2, _db = Bole.Db.commit db ~message:"changes" in
  let diffs = Bole.Db.diff db ~from:c1 ~to_:c2 ~table:"users" |> List.of_seq in
  let diff_testable = Alcotest.testable
    (fun fmt e -> match e with
      | Bole.Db.Added (k, v) ->
        Format.fprintf fmt "Added(%a, %a)" (Alcotest.pp tuple_testable) k (Alcotest.pp tuple_testable) v
      | Bole.Db.Removed (k, v) ->
        Format.fprintf fmt "Removed(%a, %a)" (Alcotest.pp tuple_testable) k (Alcotest.pp tuple_testable) v
      | Bole.Db.Modified (k, o, n) ->
        Format.fprintf fmt "Modified(%a, %a, %a)" (Alcotest.pp tuple_testable) k (Alcotest.pp tuple_testable) o (Alcotest.pp tuple_testable) n)
    (=)
  in
  Alcotest.(check (list diff_testable)) "diff shows all changes"
    [ Bole.Db.Modified ([String "alice"], [String "v1"], [String "v2"])
    ; Bole.Db.Removed ([String "bob"], [String "v1"])
    ; Bole.Db.Added ([String "carol"], [String "new"])
    ] diffs

(* --- Test 5: Clean merge --- *)

let test_clean_merge () =
  let db = Bole.Db.create () in
  let db = Bole.Db.create_table db ~table:"users" ~schema:simple_schema in
  let db = Bole.Db.put_row db ~table:"users" ~row:["key", String "alice"; "value", String "v1"] in
  let db = Bole.Db.put_row db ~table:"users" ~row:["key", String "bob"; "value", String "v1"] in
  let _base, db = Bole.Db.commit db ~message:"base" in
  let db = Bole.Db.branch db ~name:"branch-a" in
  let db = Bole.Db.put_row db ~table:"users" ~row:["key", String "alice"; "value", String "v2"] in
  let _ca, db = Bole.Db.commit db ~message:"update alice" in
  let db = Bole.Db.switch db ~name:"main" in
  let db = Bole.Db.branch db ~name:"branch-b" in
  let db = Bole.Db.put_row db ~table:"users" ~row:["key", String "bob"; "value", String "v2"] in
  let _cb, db = Bole.Db.commit db ~message:"update bob" in
  let result = Bole.Db.merge db ~ours:"branch-b" ~theirs:"branch-a" in
  Alcotest.(check int) "no conflicts" 0 (List.length result.Bole.Db.conflicts);
  let db = result.Bole.Db.db in
  (match Bole.Db.get_row db ~table:"users" ~key:[String "alice"] with
   | Some row -> Alcotest.(check tuple_value) "alice merged" (String "v2") (List.assoc "value" row)
   | None -> Alcotest.fail "alice not found");
  (match Bole.Db.get_row db ~table:"users" ~key:[String "bob"] with
   | Some row -> Alcotest.(check tuple_value) "bob merged" (String "v2") (List.assoc "value" row)
   | None -> Alcotest.fail "bob not found")

(* --- Test 6: Conflicting merge --- *)

let test_conflicting_merge () =
  let db = Bole.Db.create () in
  let db = Bole.Db.create_table db ~table:"users" ~schema:simple_schema in
  let db = Bole.Db.put_row db ~table:"users" ~row:["key", String "alice"; "value", String "v1"] in
  let _base, db = Bole.Db.commit db ~message:"base" in
  let db = Bole.Db.branch db ~name:"branch-a" in
  let db = Bole.Db.put_row db ~table:"users" ~row:["key", String "alice"; "value", String "v2"] in
  let _ca, db = Bole.Db.commit db ~message:"alice v2" in
  let db = Bole.Db.switch db ~name:"main" in
  let db = Bole.Db.branch db ~name:"branch-b" in
  let db = Bole.Db.put_row db ~table:"users" ~row:["key", String "alice"; "value", String "v3"] in
  let _cb, db = Bole.Db.commit db ~message:"alice v3" in
  let result = Bole.Db.merge db ~ours:"branch-b" ~theirs:"branch-a" in
  Alcotest.(check int) "one conflict" 1 (List.length result.Bole.Db.conflicts);
  let c = List.hd result.Bole.Db.conflicts in
  Alcotest.(check string) "conflict table" "users" c.Bole.Db.table;
  Alcotest.(check tuple_testable) "conflict key" [String "alice"] c.Bole.Db.key;
  Alcotest.(check opt_tuple) "conflict base" (Some [String "v1"]) c.Bole.Db.base;
  Alcotest.(check opt_tuple) "conflict ours" (Some [String "v3"]) c.Bole.Db.ours;
  Alcotest.(check opt_tuple) "conflict theirs" (Some [String "v2"]) c.Bole.Db.theirs

(* --- Test 7: Multi-table commits --- *)

let test_multi_table_commit () =
  let db = Bole.Db.create () in
  let db = Bole.Db.create_table db ~table:"users" ~schema:simple_schema in
  let db = Bole.Db.create_table db ~table:"posts" ~schema:simple_schema in
  let db = Bole.Db.put_row db ~table:"users" ~row:["key", String "alice"; "value", String "admin"] in
  let db = Bole.Db.put_row db ~table:"posts" ~row:["key", String "post-1"; "value", String "hello"] in
  let c1, db = Bole.Db.commit db ~message:"initial" in
  let db = Bole.Db.put_row db ~table:"users" ~row:["key", String "alice"; "value", String "editor"] in
  let c2, _db = Bole.Db.commit db ~message:"update role" in
  let diff_testable = Alcotest.testable
    (fun fmt e -> match e with
      | Bole.Db.Added (k, v) ->
        Format.fprintf fmt "Added(%a, %a)" (Alcotest.pp tuple_testable) k (Alcotest.pp tuple_testable) v
      | Bole.Db.Removed (k, v) ->
        Format.fprintf fmt "Removed(%a, %a)" (Alcotest.pp tuple_testable) k (Alcotest.pp tuple_testable) v
      | Bole.Db.Modified (k, o, n) ->
        Format.fprintf fmt "Modified(%a, %a, %a)" (Alcotest.pp tuple_testable) k (Alcotest.pp tuple_testable) o (Alcotest.pp tuple_testable) n)
    (=)
  in
  let user_diffs = Bole.Db.diff db ~from:c1 ~to_:c2 ~table:"users" |> List.of_seq in
  Alcotest.(check (list diff_testable)) "users changed"
    [Bole.Db.Modified ([String "alice"], [String "admin"], [String "editor"])] user_diffs;
  let post_diffs = Bole.Db.diff db ~from:c1 ~to_:c2 ~table:"posts" |> List.of_seq in
  Alcotest.(check (list diff_testable)) "posts unchanged" [] post_diffs;
  let old = Bole.Db.checkout db c1 in
  (match Bole.Db.get_row old ~table:"users" ~key:[String "alice"] with
   | Some row -> Alcotest.(check tuple_value) "old users restored" (String "admin") (List.assoc "value" row)
   | None -> Alcotest.fail "alice not found in old");
  (match Bole.Db.get_row old ~table:"posts" ~key:[String "post-1"] with
   | Some row -> Alcotest.(check tuple_value) "old posts restored" (String "hello") (List.assoc "value" row)
   | None -> Alcotest.fail "post-1 not found in old")

(* --- Test 8: Int64 key ordering --- *)

let scores_schema = Bole.Schema.create
  ~columns:["id", Bole.Schema.Int64; "label", Bole.Schema.Str]
  ~primary_key:["id"]

let test_int64_key_ordering () =
  let db = Bole.Db.create () in
  let db = Bole.Db.create_table db ~table:"scores" ~schema:scores_schema in
  let put db n v =
    Bole.Db.put_row db ~table:"scores" ~row:["id", Int64 n; "label", String v]
  in
  let db = put db 9L "nine" in
  let db = put db 10L "ten" in
  let db = put db 2L "two" in
  let db = put db 100L "hundred" in
  let db = put db (-1L) "neg-one" in
  let db = put db 0L "zero" in
  let all = Bole.Db.range_rows db ~table:"scores" |> List.of_seq in
  let values = List.map (fun row ->
    match List.assoc "label" row with String s -> s | _ -> assert false
  ) all in
  Alcotest.(check (list string)) "numeric order"
    ["neg-one"; "zero"; "two"; "nine"; "ten"; "hundred"] values

(* --- Test 9: Composite key ordering --- *)

let composite_schema = Bole.Schema.create
  ~columns:["name", Bole.Schema.Str; "seq", Bole.Schema.Int64; "label", Bole.Schema.Str]
  ~primary_key:["name"; "seq"]

let test_composite_key_ordering () =
  let db = Bole.Db.create () in
  let db = Bole.Db.create_table db ~table:"t" ~schema:composite_schema in
  let put db s n v =
    Bole.Db.put_row db ~table:"t" ~row:["name", String s; "seq", Int64 n; "label", String v]
  in
  let db = put db "bob" 2L "bob-2" in
  let db = put db "alice" 10L "alice-10" in
  let db = put db "alice" 2L "alice-2" in
  let db = put db "bob" 1L "bob-1" in
  let all = Bole.Db.range_rows db ~table:"t" |> List.of_seq in
  let values = List.map (fun row ->
    match List.assoc "label" row with String s -> s | _ -> assert false
  ) all in
  Alcotest.(check (list string)) "composite order"
    ["alice-2"; "alice-10"; "bob-1"; "bob-2"] values

(* --- Test 10: Schema-aware table with multi-column value --- *)

let test_schema_aware_table () =
  let db = Bole.Db.create () in
  let schema = Bole.Schema.create
    ~columns:["id", Bole.Schema.Int64; "name", Bole.Schema.Str; "email", Bole.Schema.Str]
    ~primary_key:["id"] in
  let db = Bole.Db.create_table db ~table:"users" ~schema in
  let db = Bole.Db.put_row db ~table:"users"
    ~row:["id", Int64 1L; "name", String "alice"; "email", String "alice@ex.com"] in
  let db = Bole.Db.put_row db ~table:"users"
    ~row:["id", Int64 2L; "name", String "bob"; "email", String "bob@ex.com"] in
  match Bole.Db.get_row db ~table:"users" ~key:[Int64 1L] with
  | Some row ->
    Alcotest.(check tuple_value) "name" (String "alice") (List.assoc "name" row);
    Alcotest.(check tuple_value) "email" (String "alice@ex.com") (List.assoc "email" row)
  | None -> Alcotest.fail "expected row"

(* --- Test 11: Cell-level merge --- *)

let test_cell_level_merge () =
  let db = Bole.Db.create () in
  let schema = Bole.Schema.create
    ~columns:["id", Bole.Schema.Int64; "name", Bole.Schema.Str; "email", Bole.Schema.Str]
    ~primary_key:["id"] in
  let db = Bole.Db.create_table db ~table:"users" ~schema in
  let db = Bole.Db.put_row db ~table:"users"
    ~row:["id", Int64 1L; "name", String "alice"; "email", String "old@ex.com"] in
  let _, db = Bole.Db.commit db ~message:"base" in
  let db = Bole.Db.branch db ~name:"branch-a" in
  let db = Bole.Db.put_row db ~table:"users"
    ~row:["id", Int64 1L; "name", String "Alice Smith"; "email", String "old@ex.com"] in
  let _, db = Bole.Db.commit db ~message:"fix name" in
  let db = Bole.Db.switch db ~name:"main" in
  let db = Bole.Db.branch db ~name:"branch-b" in
  let db = Bole.Db.put_row db ~table:"users"
    ~row:["id", Int64 1L; "name", String "alice"; "email", String "new@ex.com"] in
  let _, db = Bole.Db.commit db ~message:"fix email" in
  let result = Bole.Db.merge db ~ours:"branch-b" ~theirs:"branch-a" in
  Alcotest.(check int) "no conflicts" 0 (List.length result.Bole.Db.conflicts);
  match Bole.Db.get_row result.Bole.Db.db ~table:"users" ~key:[Int64 1L] with
  | Some row ->
    Alcotest.(check tuple_value) "merged name" (String "Alice Smith") (List.assoc "name" row);
    Alcotest.(check tuple_value) "merged email" (String "new@ex.com") (List.assoc "email" row)
  | None -> Alcotest.fail "expected merged row"

(* --- Registration --- *)

let tests =
  [ "acceptance", [
      Alcotest.test_case "basic table operations" `Quick test_basic_table_workflow;
      Alcotest.test_case "commit and history" `Quick test_commit_and_history;
      Alcotest.test_case "branching" `Quick test_branching;
      Alcotest.test_case "diff between commits" `Quick test_diff_branches;
      Alcotest.test_case "clean merge" `Quick test_clean_merge;
      Alcotest.test_case "conflicting merge" `Quick test_conflicting_merge;
      Alcotest.test_case "multi-table commits" `Quick test_multi_table_commit;
      Alcotest.test_case "int64 key ordering" `Quick test_int64_key_ordering;
      Alcotest.test_case "composite key ordering" `Quick test_composite_key_ordering;
      Alcotest.test_case "schema-aware table" `Quick test_schema_aware_table;
      Alcotest.test_case "cell-level merge" `Quick test_cell_level_merge;
    ]
  ]
