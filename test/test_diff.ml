let test_identical_trees () =
  let store = Bole.Store.create () in
  let pairs = List.init 10 (fun i ->
    (Printf.sprintf "key-%03d" i, Printf.sprintf "val-%03d" i)) in
  let root = Bole.Tree.build store (List.to_seq pairs) in
  let entries = Bole.Diff.diff store ~from:root ~to_:root |> List.of_seq in
  Alcotest.(check int) "no differences" 0 (List.length entries)

let test_empty_vs_nonempty () =
  let store = Bole.Store.create () in
  let empty = Bole.Tree.build store Seq.empty in
  let pairs = List.init 10 (fun i ->
    (Printf.sprintf "key-%03d" i, Printf.sprintf "val-%03d" i)) in
  let full = Bole.Tree.build store (List.to_seq pairs) in
  let added = Bole.Diff.diff store ~from:empty ~to_:full |> List.of_seq in
  Alcotest.(check int) "10 additions" 10 (List.length added);
  List.iter (fun e ->
    match e with
    | Bole.Diff.Added _ -> ()
    | _ -> Alcotest.fail "expected Added"
  ) added;
  let removed = Bole.Diff.diff store ~from:full ~to_:empty |> List.of_seq in
  Alcotest.(check int) "10 removals" 10 (List.length removed);
  List.iter (fun e ->
    match e with
    | Bole.Diff.Removed _ -> ()
    | _ -> Alcotest.fail "expected Removed"
  ) removed

let test_single_addition () =
  let store = Bole.Store.create () in
  let pairs = [("a", "1"); ("b", "2"); ("c", "3")] in
  let root = Bole.Tree.build store (List.to_seq pairs) in
  let root' = Bole.Tree.put store root "d" "4" in
  let entries = Bole.Diff.diff store ~from:root ~to_:root' |> List.of_seq in
  Alcotest.(check int) "one diff entry" 1 (List.length entries);
  match entries with
  | [Bole.Diff.Added ("d", "4")] -> ()
  | _ -> Alcotest.fail "expected Added(d, 4)"

let test_single_deletion () =
  let store = Bole.Store.create () in
  let pairs = [("a", "1"); ("b", "2"); ("c", "3")] in
  let root = Bole.Tree.build store (List.to_seq pairs) in
  let root' = Bole.Tree.delete store root "b" in
  let entries = Bole.Diff.diff store ~from:root ~to_:root' |> List.of_seq in
  Alcotest.(check int) "one diff entry" 1 (List.length entries);
  match entries with
  | [Bole.Diff.Removed ("b", "2")] -> ()
  | _ -> Alcotest.fail "expected Removed(b, 2)"

let test_single_modification () =
  let store = Bole.Store.create () in
  let pairs = [("a", "1"); ("b", "2"); ("c", "3")] in
  let root = Bole.Tree.build store (List.to_seq pairs) in
  let root' = Bole.Tree.put store root "b" "99" in
  let entries = Bole.Diff.diff store ~from:root ~to_:root' |> List.of_seq in
  Alcotest.(check int) "one diff entry" 1 (List.length entries);
  match entries with
  | [Bole.Diff.Modified ("b", "2", "99")] -> ()
  | _ -> Alcotest.fail "expected Modified(b, 2, 99)"

let test_multiple_changes () =
  let store = Bole.Store.create () in
  let pairs = [("a", "1"); ("b", "2"); ("c", "3"); ("d", "4"); ("e", "5")] in
  let root = Bole.Tree.build store (List.to_seq pairs) in
  let root' = Bole.Tree.put store root "b" "20" in       (* modify *)
  let root' = Bole.Tree.delete store root' "d" in         (* delete *)
  let root' = Bole.Tree.put store root' "cc" "new" in     (* add *)
  let entries = Bole.Diff.diff store ~from:root ~to_:root' |> List.of_seq in
  (* Expect in key order: Modified b, Added cc, Removed d *)
  Alcotest.(check int) "three diff entries" 3 (List.length entries);
  let keys = List.map (fun e ->
    match e with
    | Bole.Diff.Added (k, _) -> k
    | Bole.Diff.Removed (k, _) -> k
    | Bole.Diff.Modified (k, _, _) -> k
  ) entries in
  Alcotest.(check (list string)) "keys in order" ["b"; "cc"; "d"] keys;
  (match List.nth entries 0 with
   | Bole.Diff.Modified ("b", "2", "20") -> ()
   | _ -> Alcotest.fail "expected Modified(b)");
  (match List.nth entries 1 with
   | Bole.Diff.Added ("cc", "new") -> ()
   | _ -> Alcotest.fail "expected Added(cc)");
  (match List.nth entries 2 with
   | Bole.Diff.Removed ("d", "4") -> ()
   | _ -> Alcotest.fail "expected Removed(d)")

let test_multi_chunk_diff () =
  let store = Bole.Store.create () in
  let pairs = List.init 200 (fun i ->
    (Printf.sprintf "key-%05d" i, Printf.sprintf "val-%05d" i)) in
  let root = Bole.Tree.build ~target_size:20 store (List.to_seq pairs) in
  (* Apply several mutations *)
  let root' = Bole.Tree.put ~target_size:20 store root "key-00050" "changed" in
  let root' = Bole.Tree.delete ~target_size:20 store root' "key-00100" in
  let root' = Bole.Tree.put ~target_size:20 store root' "key-00200" "new-entry" in
  let entries = Bole.Diff.diff store ~from:root ~to_:root' |> List.of_seq in
  Alcotest.(check int) "three diff entries" 3 (List.length entries);
  (* Verify each entry *)
  let has e = List.mem e entries in
  Alcotest.(check bool) "modified key-00050"
    true (has (Bole.Diff.Modified ("key-00050", "val-00050", "changed")));
  Alcotest.(check bool) "removed key-00100"
    true (has (Bole.Diff.Removed ("key-00100", "val-00100")));
  Alcotest.(check bool) "added key-00200"
    true (has (Bole.Diff.Added ("key-00200", "new-entry")));
  (* Verify key ordering *)
  let keys = List.map (fun e ->
    match e with
    | Bole.Diff.Added (k, _) -> k
    | Bole.Diff.Removed (k, _) -> k
    | Bole.Diff.Modified (k, _, _) -> k
  ) entries in
  let sorted = List.sort String.compare keys in
  Alcotest.(check (list string)) "keys in sorted order" sorted keys

let test_key_ordering () =
  let store = Bole.Store.create () in
  let pairs = List.init 50 (fun i ->
    (Printf.sprintf "k-%03d" i, Printf.sprintf "v-%03d" i)) in
  let root = Bole.Tree.build ~target_size:20 store (List.to_seq pairs) in
  let empty = Bole.Tree.build store Seq.empty in
  let entries = Bole.Diff.diff store ~from:empty ~to_:root |> List.of_seq in
  let keys = List.map (fun e ->
    match e with
    | Bole.Diff.Added (k, _) -> k
    | _ -> Alcotest.fail "expected Added"
  ) entries in
  let sorted = List.sort String.compare keys in
  Alcotest.(check (list string)) "all entries in key order" sorted keys

let diff_entry_key = function
  | Bole.Diff.Added (k, _) -> k
  | Bole.Diff.Removed (k, _) -> k
  | Bole.Diff.Modified (k, _, _) -> k

let prop_diff_completeness =
  QCheck2.Test.make ~name:"diff captures exactly the applied changes"
    ~count:20
    QCheck2.Gen.(pair
      (list_size (int_range 10 80)
        (pair
          (string_size ~gen:printable (int_range 1 10))
          (string_size ~gen:printable (int_range 1 10))))
      (list_size (int_range 1 20)
        (pair
          (string_size ~gen:printable (int_range 1 10))
          (string_size ~gen:printable (int_range 1 10)))))
    (fun (initial_pairs, mutations) ->
       let sorted = List.sort_uniq (fun (k1, _) (k2, _) ->
         String.compare k1 k2) initial_pairs in
       if List.length sorted < 5 then true
       else begin
         let store = Bole.Store.create () in
         let root_a = Bole.Tree.build ~target_size:20 store (List.to_seq sorted) in
         (* Apply mutations: treat each as a put *)
         let root_b = List.fold_left (fun r (k, v) ->
           Bole.Tree.put ~target_size:20 store r k v
         ) root_a mutations in
         let diff_entries = Bole.Diff.diff store ~from:root_a ~to_:root_b
           |> List.of_seq in
         (* Every diff entry should reflect a real difference *)
         List.for_all (fun e ->
           match e with
           | Bole.Diff.Added (k, v) ->
             Bole.Tree.find store root_a k = None
             && Bole.Tree.find store root_b k = Some v
           | Bole.Diff.Removed (k, v) ->
             Bole.Tree.find store root_a k = Some v
             && Bole.Tree.find store root_b k = None
           | Bole.Diff.Modified (k, old_v, new_v) ->
             Bole.Tree.find store root_a k = Some old_v
             && Bole.Tree.find store root_b k = Some new_v
             && old_v <> new_v
         ) diff_entries
         (* And every actual difference should appear in the diff *)
         && begin
           let all_keys = List.sort_uniq String.compare
             (List.map fst sorted @ List.map fst mutations) in
           List.for_all (fun k ->
             let in_a = Bole.Tree.find store root_a k in
             let in_b = Bole.Tree.find store root_b k in
             match in_a, in_b with
             | None, None -> true
             | Some _, None ->
               List.exists (fun e -> diff_entry_key e = k) diff_entries
             | None, Some _ ->
               List.exists (fun e -> diff_entry_key e = k) diff_entries
             | Some va, Some vb ->
               if va = vb then
                 not (List.exists (fun e -> diff_entry_key e = k) diff_entries)
               else
                 List.exists (fun e -> diff_entry_key e = k) diff_entries
           ) all_keys
         end
       end)

let prop_diff_symmetry =
  QCheck2.Test.make ~name:"diff from/to is mirror of diff to/from"
    ~count:20
    QCheck2.Gen.(list_size (int_range 5 50)
      (pair
        (string_size ~gen:printable (int_range 1 10))
        (string_size ~gen:printable (int_range 1 10))))
    (fun pairs ->
       let sorted = List.sort_uniq (fun (k1, _) (k2, _) ->
         String.compare k1 k2) pairs in
       if List.length sorted < 3 then true
       else begin
         let n = List.length sorted in
         let half = n / 2 in
         let pairs_a = List.filteri (fun i _ -> i < half + half / 2) sorted in
         let pairs_b = List.filteri (fun i _ -> i >= half / 2) sorted in
         (* Modify some overlapping values *)
         let pairs_b = List.map (fun (k, v) ->
           if String.length k > 0 && Char.code k.[0] mod 3 = 0
           then (k, v ^ "-modified")
           else (k, v)
         ) pairs_b in
         let pairs_b = List.sort_uniq (fun (k1, _) (k2, _) ->
           String.compare k1 k2) pairs_b in
         let store = Bole.Store.create () in
         let root_a = Bole.Tree.build ~target_size:20 store (List.to_seq pairs_a) in
         let root_b = Bole.Tree.build ~target_size:20 store (List.to_seq pairs_b) in
         let forward = Bole.Diff.diff store ~from:root_a ~to_:root_b
           |> List.of_seq in
         let backward = Bole.Diff.diff store ~from:root_b ~to_:root_a
           |> List.of_seq in
         let mirror = function
           | Bole.Diff.Added (k, v) -> Bole.Diff.Removed (k, v)
           | Bole.Diff.Removed (k, v) -> Bole.Diff.Added (k, v)
           | Bole.Diff.Modified (k, o, n) -> Bole.Diff.Modified (k, n, o)
         in
         let mirrored = List.map mirror forward in
         List.length mirrored = List.length backward
         && List.for_all2 (fun a b ->
           diff_entry_key a = diff_entry_key b && a = b
         ) (List.sort (fun a b ->
              String.compare (diff_entry_key a) (diff_entry_key b)) mirrored)
            (List.sort (fun a b ->
              String.compare (diff_entry_key a) (diff_entry_key b)) backward)
       end)

let prop_apply_diff_round_trip =
  QCheck2.Test.make ~name:"applying diff to source produces target"
    ~count:20
    QCheck2.Gen.(pair
      (list_size (int_range 10 80)
        (pair
          (string_size ~gen:printable (int_range 1 10))
          (string_size ~gen:printable (int_range 1 10))))
      (list_size (int_range 1 15)
        (pair
          (string_size ~gen:printable (int_range 1 10))
          (string_size ~gen:printable (int_range 1 10)))))
    (fun (initial_pairs, mutations) ->
       let sorted = List.sort_uniq (fun (k1, _) (k2, _) ->
         String.compare k1 k2) initial_pairs in
       if List.length sorted < 5 then true
       else begin
         let store = Bole.Store.create () in
         let root_a = Bole.Tree.build ~target_size:20 store (List.to_seq sorted) in
         let root_b = List.fold_left (fun r (k, v) ->
           Bole.Tree.put ~target_size:20 store r k v
         ) root_a mutations in
         let diff_entries = Bole.Diff.diff store ~from:root_a ~to_:root_b
           |> List.of_seq in
         (* Apply diff to root_a *)
         let root_applied = List.fold_left (fun r e ->
           match e with
           | Bole.Diff.Added (k, v) ->
             Bole.Tree.put ~target_size:20 store r k v
           | Bole.Diff.Removed (k, _) ->
             Bole.Tree.delete ~target_size:20 store r k
           | Bole.Diff.Modified (k, _, new_v) ->
             Bole.Tree.put ~target_size:20 store r k new_v
         ) root_a diff_entries in
         (* root_applied should have same contents as root_b *)
         let entries_applied = Bole.Tree.range store root_applied |> List.of_seq in
         let entries_b = Bole.Tree.range store root_b |> List.of_seq in
         entries_applied = entries_b
       end)

let tests =
  [ "diff", [
      Alcotest.test_case "identical trees" `Quick test_identical_trees;
      Alcotest.test_case "empty vs non-empty" `Quick test_empty_vs_nonempty;
      Alcotest.test_case "single addition" `Quick test_single_addition;
      Alcotest.test_case "single deletion" `Quick test_single_deletion;
      Alcotest.test_case "single modification" `Quick test_single_modification;
      Alcotest.test_case "multiple changes" `Quick test_multiple_changes;
      Alcotest.test_case "multi-chunk diff" `Quick test_multi_chunk_diff;
      Alcotest.test_case "key ordering" `Quick test_key_ordering;
      QCheck_alcotest.to_alcotest prop_diff_completeness;
      QCheck_alcotest.to_alcotest prop_diff_symmetry;
      QCheck_alcotest.to_alcotest prop_apply_diff_round_trip;
    ]
  ]
