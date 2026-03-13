let test_deterministic () =
  let keys = List.init 200 (fun i -> Printf.sprintf "key-%05d" i) in
  let run () =
    let c = Bole.Chunker.create ~target_size:20 ~level:0 in
    List.filter_map (fun key ->
      if Bole.Chunker.feed c ~key then begin
        let n = Bole.Chunker.count c in
        Bole.Chunker.reset c;
        Some n
      end else
        None
    ) keys
  in
  let sizes1 = run () in
  let sizes2 = run () in
  Alcotest.(check (list int)) "deterministic boundaries" sizes1 sizes2

let test_respects_min_size () =
  let c = Bole.Chunker.create ~target_size:20 ~level:0 in
  let min_size = 20 / 4 in
  let violation = ref false in
  for i = 0 to 999 do
    let key = Printf.sprintf "key-%05d" i in
    if Bole.Chunker.feed c ~key then begin
      if Bole.Chunker.count c < min_size then
        violation := true;
      Bole.Chunker.reset c
    end
  done;
  Alcotest.(check bool) "no chunk below min_size" false !violation

let test_count_increments () =
  let c = Bole.Chunker.create ~target_size:100 ~level:0 in
  Alcotest.(check int) "count starts at 0" 0 (Bole.Chunker.count c);
  ignore (Bole.Chunker.feed c ~key:"a");
  Alcotest.(check int) "count after 1 feed" 1 (Bole.Chunker.count c);
  ignore (Bole.Chunker.feed c ~key:"b");
  Alcotest.(check int) "count after 2 feeds" 2 (Bole.Chunker.count c)

let test_reset_clears_count () =
  let c = Bole.Chunker.create ~target_size:100 ~level:0 in
  ignore (Bole.Chunker.feed c ~key:"a");
  ignore (Bole.Chunker.feed c ~key:"b");
  Bole.Chunker.reset c;
  Alcotest.(check int) "count after reset" 0 (Bole.Chunker.count c)

let prop_mean_chunk_size =
  QCheck2.Test.make ~name:"mean chunk size near target"
    ~count:20
    QCheck2.Gen.(int_range 10 100)
    (fun target_size ->
       let c = Bole.Chunker.create ~target_size ~level:0 in
       let chunk_sizes = ref [] in
       let total_keys = target_size * 100 in
       for i = 0 to total_keys - 1 do
         let key = Printf.sprintf "key-%08d" i in
         if Bole.Chunker.feed c ~key then begin
           chunk_sizes := Bole.Chunker.count c :: !chunk_sizes;
           Bole.Chunker.reset c
         end
       done;
       match !chunk_sizes with
       | [] -> true (* no boundaries = one big chunk, can happen with large target *)
       | sizes ->
         let sum = List.fold_left ( + ) 0 sizes in
         let mean = float_of_int sum /. float_of_int (List.length sizes) in
         let ratio = mean /. float_of_int target_size in
         (* Mean should be within 50% of target *)
         ratio > 0.5 && ratio < 2.0)

let prop_no_tiny_chunks =
  QCheck2.Test.make ~name:"no chunks below min_size"
    ~count:20
    QCheck2.Gen.(int_range 10 100)
    (fun target_size ->
       let c = Bole.Chunker.create ~target_size ~level:0 in
       let min_size = max 1 (target_size / 4) in
       let ok = ref true in
       for i = 0 to target_size * 100 - 1 do
         let key = Printf.sprintf "key-%08d" i in
         if Bole.Chunker.feed c ~key then begin
           if Bole.Chunker.count c < min_size then ok := false;
           Bole.Chunker.reset c
         end
       done;
       !ok)

let test_level_salting () =
  let keys = List.init 200 (fun i -> Printf.sprintf "key-%05d" i) in
  let boundaries_at level =
    let c = Bole.Chunker.create ~target_size:20 ~level in
    let acc = ref [] in
    List.iteri (fun i key ->
      if Bole.Chunker.feed c ~key then begin
        acc := i :: !acc;
        Bole.Chunker.reset c
      end
    ) keys;
    List.rev !acc
  in
  let b0 = boundaries_at 0 in
  let b1 = boundaries_at 1 in
  Alcotest.(check bool) "different levels produce different boundaries"
    true (b0 <> b1)

let tests =
  [ "chunker", [
      Alcotest.test_case "deterministic" `Quick test_deterministic;
      Alcotest.test_case "respects min_size" `Quick test_respects_min_size;
      Alcotest.test_case "count increments" `Quick test_count_increments;
      Alcotest.test_case "reset clears count" `Quick test_reset_clears_count;
      Alcotest.test_case "level salting" `Quick test_level_salting;
      QCheck_alcotest.to_alcotest prop_mean_chunk_size;
      QCheck_alcotest.to_alcotest prop_no_tiny_chunks;
    ]
  ]
