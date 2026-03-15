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

(* --- Test 1: Basic table operations --- *)

let test_basic_table_workflow () =
  let db = Bole.Db.create () in
  let db = Bole.Db.put db ~table:"users" ~key:[String "alice"] ~value:[String "admin"] in
  let db = Bole.Db.put db ~table:"users" ~key:[String "bob"] ~value:[String "editor"] in
  Alcotest.(check opt_tuple) "find alice"
    (Some [String "admin"]) (Bole.Db.find db ~table:"users" ~key:[String "alice"]);
  Alcotest.(check opt_tuple) "find bob"
    (Some [String "editor"]) (Bole.Db.find db ~table:"users" ~key:[String "bob"]);
  Alcotest.(check opt_tuple) "different table is independent"
    None (Bole.Db.find db ~table:"posts" ~key:[String "alice"])

(* --- Test 2: Commit and history --- *)

let test_commit_and_history () =
  let db = Bole.Db.create () in
  let db = Bole.Db.put db ~table:"users" ~key:[String "alice"] ~value:[String "v1"] in
  let commit1, db = Bole.Db.commit db ~message:"first" in
  let db = Bole.Db.put db ~table:"users" ~key:[String "alice"] ~value:[String "v2"] in
  let commit2, _db = Bole.Db.commit db ~message:"second" in
  Alcotest.(check opt_tuple) "current state sees v2"
    (Some [String "v2"]) (Bole.Db.find db ~table:"users" ~key:[String "alice"]);
  let old = Bole.Db.checkout db commit1 in
  Alcotest.(check opt_tuple) "old commit sees v1"
    (Some [String "v1"]) (Bole.Db.find old ~table:"users" ~key:[String "alice"]);
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
  let db = Bole.Db.put db ~table:"users" ~key:[String "alice"] ~value:[String "v1"] in
  let _c1, db = Bole.Db.commit db ~message:"initial" in
  let db = Bole.Db.branch db ~name:"feature" in
  let db = Bole.Db.put db ~table:"users" ~key:[String "bob"] ~value:[String "new"] in
  let _c2, db = Bole.Db.commit db ~message:"add bob on feature" in
  let db = Bole.Db.switch db ~name:"main" in
  Alcotest.(check opt_tuple) "main doesn't see bob"
    None (Bole.Db.find db ~table:"users" ~key:[String "bob"]);
  Alcotest.(check opt_tuple) "main sees alice"
    (Some [String "v1"]) (Bole.Db.find db ~table:"users" ~key:[String "alice"]);
  let db = Bole.Db.switch db ~name:"feature" in
  Alcotest.(check opt_tuple) "feature sees bob"
    (Some [String "new"]) (Bole.Db.find db ~table:"users" ~key:[String "bob"]);
  Alcotest.(check opt_tuple) "feature sees alice"
    (Some [String "v1"]) (Bole.Db.find db ~table:"users" ~key:[String "alice"])

(* --- Test 4: Diff between commits --- *)

let test_diff_branches () =
  let db = Bole.Db.create () in
  let db = Bole.Db.put db ~table:"users" ~key:[String "alice"] ~value:[String "v1"] in
  let db = Bole.Db.put db ~table:"users" ~key:[String "bob"] ~value:[String "v1"] in
  let c1, db = Bole.Db.commit db ~message:"initial" in
  let db = Bole.Db.put db ~table:"users" ~key:[String "alice"] ~value:[String "v2"] in
  let db = Bole.Db.delete db ~table:"users" ~key:[String "bob"] in
  let db = Bole.Db.put db ~table:"users" ~key:[String "carol"] ~value:[String "new"] in
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
  let db = Bole.Db.put db ~table:"users" ~key:[String "alice"] ~value:[String "v1"] in
  let db = Bole.Db.put db ~table:"users" ~key:[String "bob"] ~value:[String "v1"] in
  let _base, db = Bole.Db.commit db ~message:"base" in
  let db = Bole.Db.branch db ~name:"branch-a" in
  let db = Bole.Db.put db ~table:"users" ~key:[String "alice"] ~value:[String "v2"] in
  let _ca, db = Bole.Db.commit db ~message:"update alice" in
  let db = Bole.Db.switch db ~name:"main" in
  let db = Bole.Db.branch db ~name:"branch-b" in
  let db = Bole.Db.put db ~table:"users" ~key:[String "bob"] ~value:[String "v2"] in
  let _cb, db = Bole.Db.commit db ~message:"update bob" in
  let result = Bole.Db.merge db ~ours:"branch-b" ~theirs:"branch-a" in
  Alcotest.(check int) "no conflicts" 0 (List.length result.Bole.Db.conflicts);
  let db = result.Bole.Db.db in
  Alcotest.(check opt_tuple) "alice merged"
    (Some [String "v2"]) (Bole.Db.find db ~table:"users" ~key:[String "alice"]);
  Alcotest.(check opt_tuple) "bob merged"
    (Some [String "v2"]) (Bole.Db.find db ~table:"users" ~key:[String "bob"])

(* --- Test 6: Conflicting merge --- *)

let test_conflicting_merge () =
  let db = Bole.Db.create () in
  let db = Bole.Db.put db ~table:"users" ~key:[String "alice"] ~value:[String "v1"] in
  let _base, db = Bole.Db.commit db ~message:"base" in
  let db = Bole.Db.branch db ~name:"branch-a" in
  let db = Bole.Db.put db ~table:"users" ~key:[String "alice"] ~value:[String "v2"] in
  let _ca, db = Bole.Db.commit db ~message:"alice v2" in
  let db = Bole.Db.switch db ~name:"main" in
  let db = Bole.Db.branch db ~name:"branch-b" in
  let db = Bole.Db.put db ~table:"users" ~key:[String "alice"] ~value:[String "v3"] in
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
  let db = Bole.Db.put db ~table:"users" ~key:[String "alice"] ~value:[String "admin"] in
  let db = Bole.Db.put db ~table:"posts" ~key:[String "post-1"] ~value:[String "hello"] in
  let c1, db = Bole.Db.commit db ~message:"initial" in
  let db = Bole.Db.put db ~table:"users" ~key:[String "alice"] ~value:[String "editor"] in
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
  Alcotest.(check opt_tuple) "old users restored"
    (Some [String "admin"]) (Bole.Db.find old ~table:"users" ~key:[String "alice"]);
  Alcotest.(check opt_tuple) "old posts restored"
    (Some [String "hello"]) (Bole.Db.find old ~table:"posts" ~key:[String "post-1"])

(* --- Test 8: Int64 key ordering --- *)

let test_int64_key_ordering () =
  let db = Bole.Db.create () in
  let put db n v =
    Bole.Db.put db ~table:"scores" ~key:[Int64 n] ~value:[String v]
  in
  let db = put db 9L "nine" in
  let db = put db 10L "ten" in
  let db = put db 2L "two" in
  let db = put db 100L "hundred" in
  let db = put db (-1L) "neg-one" in
  let db = put db 0L "zero" in
  let all = Bole.Db.range db ~table:"scores" |> List.of_seq in
  let values = List.map (fun (_, v) ->
    match v with [String s] -> s | _ -> assert false
  ) all in
  Alcotest.(check (list string)) "numeric order"
    ["neg-one"; "zero"; "two"; "nine"; "ten"; "hundred"] values

(* --- Test 9: Composite key ordering --- *)

let test_composite_key_ordering () =
  let db = Bole.Db.create () in
  let put db s n v =
    Bole.Db.put db ~table:"t" ~key:[String s; Int64 n] ~value:[String v]
  in
  let db = put db "bob" 2L "bob-2" in
  let db = put db "alice" 10L "alice-10" in
  let db = put db "alice" 2L "alice-2" in
  let db = put db "bob" 1L "bob-1" in
  let all = Bole.Db.range db ~table:"t" |> List.of_seq in
  let values = List.map (fun (_, v) ->
    match v with [String s] -> s | _ -> assert false
  ) all in
  Alcotest.(check (list string)) "composite order"
    ["alice-2"; "alice-10"; "bob-1"; "bob-2"] values

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
    ]
  ]
