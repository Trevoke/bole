let test_identical_trees () =
  let store = Bole.Store.create () in
  let pairs = List.init 10 (fun i ->
    (Printf.sprintf "key-%03d" i, Printf.sprintf "val-%03d" i)) in
  let root = Bole.Tree.build store (List.to_seq pairs) in
  let entries = Bole.Diff.diff store ~from:root ~to_:root |> List.of_seq in
  Alcotest.(check int) "no differences" 0 (List.length entries)

let tests =
  [ "diff", [
      Alcotest.test_case "identical trees" `Quick test_identical_trees;
    ]
  ]
