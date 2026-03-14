let test_both_empty () =
  let result = Bole.Merge.three_way ~ours:Seq.empty ~theirs:Seq.empty in
  Alcotest.(check int) "no changes" 0 (List.length result.Bole.Merge.changes);
  Alcotest.(check int) "no conflicts" 0 (List.length result.Bole.Merge.conflicts)

let test_only_theirs () =
  let theirs = List.to_seq [
    Bole.Diff.Added ("b", "val-b");
    Bole.Diff.Modified ("c", "old-c", "new-c");
    Bole.Diff.Removed ("d", "val-d");
  ] in
  let result = Bole.Merge.three_way ~ours:Seq.empty ~theirs in
  Alcotest.(check int) "3 changes" 3 (List.length result.Bole.Merge.changes);
  Alcotest.(check int) "no conflicts" 0 (List.length result.Bole.Merge.conflicts);
  (match result.Bole.Merge.changes with
   | [Bole.Merge.Put ("b", "val-b");
      Bole.Merge.Put ("c", "new-c");
      Bole.Merge.Delete "d"] -> ()
   | _ -> Alcotest.fail "unexpected changes")

let test_only_ours () =
  let ours = List.to_seq [
    Bole.Diff.Added ("a", "val-a");
    Bole.Diff.Modified ("b", "old", "new");
  ] in
  let result = Bole.Merge.three_way ~ours ~theirs:Seq.empty in
  Alcotest.(check int) "no changes" 0 (List.length result.Bole.Merge.changes);
  Alcotest.(check int) "no conflicts" 0 (List.length result.Bole.Merge.conflicts)

let test_same_change () =
  let entry = Bole.Diff.Modified ("a", "old", "new") in
  let ours = List.to_seq [entry] in
  let theirs = List.to_seq [entry] in
  let result = Bole.Merge.three_way ~ours ~theirs in
  Alcotest.(check int) "no changes" 0 (List.length result.Bole.Merge.changes);
  Alcotest.(check int) "no conflicts" 0 (List.length result.Bole.Merge.conflicts)

let test_different_modify () =
  let ours = List.to_seq [Bole.Diff.Modified ("a", "base", "ours-val")] in
  let theirs = List.to_seq [Bole.Diff.Modified ("a", "base", "theirs-val")] in
  let result = Bole.Merge.three_way ~ours ~theirs in
  Alcotest.(check int) "no changes" 0 (List.length result.Bole.Merge.changes);
  Alcotest.(check int) "one conflict" 1 (List.length result.Bole.Merge.conflicts);
  let c = List.hd result.Bole.Merge.conflicts in
  Alcotest.(check string) "key" "a" c.Bole.Merge.key;
  Alcotest.(check (option string)) "base" (Some "base") c.Bole.Merge.base;
  Alcotest.(check (option string)) "ours" (Some "ours-val") c.Bole.Merge.ours;
  Alcotest.(check (option string)) "theirs" (Some "theirs-val") c.Bole.Merge.theirs

let test_both_added_different () =
  let ours = List.to_seq [Bole.Diff.Added ("a", "v1")] in
  let theirs = List.to_seq [Bole.Diff.Added ("a", "v2")] in
  let result = Bole.Merge.three_way ~ours ~theirs in
  Alcotest.(check int) "no changes" 0 (List.length result.Bole.Merge.changes);
  Alcotest.(check int) "one conflict" 1 (List.length result.Bole.Merge.conflicts);
  let c = List.hd result.Bole.Merge.conflicts in
  Alcotest.(check (option string)) "base" None c.Bole.Merge.base;
  Alcotest.(check (option string)) "ours" (Some "v1") c.Bole.Merge.ours;
  Alcotest.(check (option string)) "theirs" (Some "v2") c.Bole.Merge.theirs

let test_mixed () =
  let ours = List.to_seq [
    Bole.Diff.Modified ("a", "old-a", "ours-a");
    Bole.Diff.Added ("c", "ours-c");
    Bole.Diff.Modified ("d", "old-d", "new-d");
  ] in
  let theirs = List.to_seq [
    Bole.Diff.Added ("b", "theirs-b");
    Bole.Diff.Added ("c", "theirs-c");
    Bole.Diff.Modified ("d", "old-d", "new-d");
  ] in
  let result = Bole.Merge.three_way ~ours ~theirs in
  Alcotest.(check int) "1 change" 1 (List.length result.Bole.Merge.changes);
  (match result.Bole.Merge.changes with
   | [Bole.Merge.Put ("b", "theirs-b")] -> ()
   | _ -> Alcotest.fail "expected Put(b)");
  Alcotest.(check int) "1 conflict" 1 (List.length result.Bole.Merge.conflicts);
  Alcotest.(check string) "conflict key" "c"
    (List.hd result.Bole.Merge.conflicts).Bole.Merge.key

let tests =
  [ "merge", [
      Alcotest.test_case "both empty" `Quick test_both_empty;
      Alcotest.test_case "only theirs" `Quick test_only_theirs;
      Alcotest.test_case "only ours" `Quick test_only_ours;
      Alcotest.test_case "same change" `Quick test_same_change;
      Alcotest.test_case "different modify" `Quick test_different_modify;
      Alcotest.test_case "both added different" `Quick test_both_added_different;
      Alcotest.test_case "mixed" `Quick test_mixed;
    ]
  ]
