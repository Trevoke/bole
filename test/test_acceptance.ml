(* Acceptance tests for bole as a diffable, mergeable database.
   These tests define the user-facing behavior we're building toward.
   They will fail until the Db module and supporting layers are implemented. *)

(* --- Test 1: Basic table operations --- *)

let test_basic_table_workflow () =
  let db = Bole.Db.create () in
  let db = Bole.Db.put db ~table:"users" ~key:"alice" ~value:"admin" in
  let db = Bole.Db.put db ~table:"users" ~key:"bob" ~value:"editor" in
  Alcotest.(check (option string)) "find alice"
    (Some "admin") (Bole.Db.find db ~table:"users" ~key:"alice");
  Alcotest.(check (option string)) "find bob"
    (Some "editor") (Bole.Db.find db ~table:"users" ~key:"bob");
  Alcotest.(check (option string)) "different table is independent"
    None (Bole.Db.find db ~table:"posts" ~key:"alice")

(* --- Test 2: Commit and history --- *)

let test_commit_and_history () =
  let db = Bole.Db.create () in
  let db = Bole.Db.put db ~table:"users" ~key:"alice" ~value:"v1" in
  let commit1, db = Bole.Db.commit db ~message:"first" in
  let db = Bole.Db.put db ~table:"users" ~key:"alice" ~value:"v2" in
  let commit2, _db = Bole.Db.commit db ~message:"second" in
  Alcotest.(check (option string)) "current state sees v2"
    (Some "v2") (Bole.Db.find db ~table:"users" ~key:"alice");
  let old = Bole.Db.checkout db commit1 in
  Alcotest.(check (option string)) "old commit sees v1"
    (Some "v1") (Bole.Db.find old ~table:"users" ~key:"alice");
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
  let db = Bole.Db.put db ~table:"users" ~key:"alice" ~value:"v1" in
  let _c1, db = Bole.Db.commit db ~message:"initial" in
  let db = Bole.Db.branch db ~name:"feature" in
  let db = Bole.Db.put db ~table:"users" ~key:"bob" ~value:"new" in
  let _c2, db = Bole.Db.commit db ~message:"add bob on feature" in
  let db = Bole.Db.switch db ~name:"main" in
  Alcotest.(check (option string)) "main doesn't see bob"
    None (Bole.Db.find db ~table:"users" ~key:"bob");
  Alcotest.(check (option string)) "main sees alice"
    (Some "v1") (Bole.Db.find db ~table:"users" ~key:"alice");
  let db = Bole.Db.switch db ~name:"feature" in
  Alcotest.(check (option string)) "feature sees bob"
    (Some "new") (Bole.Db.find db ~table:"users" ~key:"bob");
  Alcotest.(check (option string)) "feature sees alice"
    (Some "v1") (Bole.Db.find db ~table:"users" ~key:"alice")

(* --- Test 4: Diff between commits --- *)

let test_diff_branches () =
  let db = Bole.Db.create () in
  let db = Bole.Db.put db ~table:"users" ~key:"alice" ~value:"v1" in
  let db = Bole.Db.put db ~table:"users" ~key:"bob" ~value:"v1" in
  let c1, db = Bole.Db.commit db ~message:"initial" in
  let db = Bole.Db.put db ~table:"users" ~key:"alice" ~value:"v2" in
  let db = Bole.Db.delete db ~table:"users" ~key:"bob" in
  let db = Bole.Db.put db ~table:"users" ~key:"carol" ~value:"new" in
  let c2, _db = Bole.Db.commit db ~message:"changes" in
  let diffs = Bole.Db.diff db ~from:c1 ~to_:c2 ~table:"users" |> List.of_seq in
  let diff_testable = Alcotest.testable
    (fun fmt e -> match e with
      | Bole.Diff.Added (k, v) ->
        Format.fprintf fmt "Added(%s, %s)" k v
      | Bole.Diff.Removed (k, v) ->
        Format.fprintf fmt "Removed(%s, %s)" k v
      | Bole.Diff.Modified (k, o, n) ->
        Format.fprintf fmt "Modified(%s, %s, %s)" k o n)
    (=)
  in
  Alcotest.(check (list diff_testable)) "diff shows all changes"
    [ Bole.Diff.Modified ("alice", "v1", "v2")
    ; Bole.Diff.Removed ("bob", "v1")
    ; Bole.Diff.Added ("carol", "new")
    ] diffs

(* --- Test 5: Clean merge --- *)

let test_clean_merge () =
  let db = Bole.Db.create () in
  let db = Bole.Db.put db ~table:"users" ~key:"alice" ~value:"v1" in
  let db = Bole.Db.put db ~table:"users" ~key:"bob" ~value:"v1" in
  let _base, db = Bole.Db.commit db ~message:"base" in
  let db = Bole.Db.branch db ~name:"branch-a" in
  let db = Bole.Db.put db ~table:"users" ~key:"alice" ~value:"v2" in
  let _ca, db = Bole.Db.commit db ~message:"update alice" in
  let db = Bole.Db.switch db ~name:"main" in
  let db = Bole.Db.branch db ~name:"branch-b" in
  let db = Bole.Db.put db ~table:"users" ~key:"bob" ~value:"v2" in
  let _cb, db = Bole.Db.commit db ~message:"update bob" in
  let result = Bole.Db.merge db ~ours:"branch-b" ~theirs:"branch-a" in
  match result with
  | Ok db ->
    Alcotest.(check (option string)) "alice merged"
      (Some "v2") (Bole.Db.find db ~table:"users" ~key:"alice");
    Alcotest.(check (option string)) "bob merged"
      (Some "v2") (Bole.Db.find db ~table:"users" ~key:"bob")
  | Error _ -> Alcotest.fail "expected clean merge"

(* --- Test 6: Conflicting merge --- *)

let test_conflicting_merge () =
  let db = Bole.Db.create () in
  let db = Bole.Db.put db ~table:"users" ~key:"alice" ~value:"v1" in
  let _base, db = Bole.Db.commit db ~message:"base" in
  let db = Bole.Db.branch db ~name:"branch-a" in
  let db = Bole.Db.put db ~table:"users" ~key:"alice" ~value:"v2" in
  let _ca, db = Bole.Db.commit db ~message:"alice v2" in
  let db = Bole.Db.switch db ~name:"main" in
  let db = Bole.Db.branch db ~name:"branch-b" in
  let db = Bole.Db.put db ~table:"users" ~key:"alice" ~value:"v3" in
  let _cb, db = Bole.Db.commit db ~message:"alice v3" in
  let result = Bole.Db.merge db ~ours:"branch-b" ~theirs:"branch-a" in
  match result with
  | Ok _ -> Alcotest.fail "expected conflict"
  | Error conflicts ->
    Alcotest.(check int) "one conflict" 1 (List.length conflicts);
    let c = List.hd conflicts in
    Alcotest.(check string) "conflict table" "users" c.Bole.Db.table;
    Alcotest.(check string) "conflict key" "alice" c.Bole.Db.key;
    Alcotest.(check string) "conflict base" "v1" c.Bole.Db.base;
    Alcotest.(check string) "conflict ours" "v3" c.Bole.Db.ours;
    Alcotest.(check string) "conflict theirs" "v2" c.Bole.Db.theirs

(* --- Test 7: Multi-table commits --- *)

let test_multi_table_commit () =
  let db = Bole.Db.create () in
  let db = Bole.Db.put db ~table:"users" ~key:"alice" ~value:"admin" in
  let db = Bole.Db.put db ~table:"posts" ~key:"post-1" ~value:"hello" in
  let c1, db = Bole.Db.commit db ~message:"initial" in
  let db = Bole.Db.put db ~table:"users" ~key:"alice" ~value:"editor" in
  let c2, _db = Bole.Db.commit db ~message:"update role" in
  let diff_testable = Alcotest.testable
    (fun fmt e -> match e with
      | Bole.Diff.Added (k, v) ->
        Format.fprintf fmt "Added(%s, %s)" k v
      | Bole.Diff.Removed (k, v) ->
        Format.fprintf fmt "Removed(%s, %s)" k v
      | Bole.Diff.Modified (k, o, n) ->
        Format.fprintf fmt "Modified(%s, %s, %s)" k o n)
    (=)
  in
  let user_diffs = Bole.Db.diff db ~from:c1 ~to_:c2 ~table:"users" |> List.of_seq in
  Alcotest.(check (list diff_testable)) "users changed"
    [Bole.Diff.Modified ("alice", "admin", "editor")] user_diffs;
  let post_diffs = Bole.Db.diff db ~from:c1 ~to_:c2 ~table:"posts" |> List.of_seq in
  Alcotest.(check (list diff_testable)) "posts unchanged" [] post_diffs;
  let old = Bole.Db.checkout db c1 in
  Alcotest.(check (option string)) "old users restored"
    (Some "admin") (Bole.Db.find old ~table:"users" ~key:"alice");
  Alcotest.(check (option string)) "old posts restored"
    (Some "hello") (Bole.Db.find old ~table:"posts" ~key:"post-1")

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
    ]
  ]
