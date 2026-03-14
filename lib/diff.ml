type entry =
  | Added of string * string
  | Removed of string * string
  | Modified of string * string * string

let rec merge_leaves left right () =
  match left, right with
  | [], [] -> Seq.Nil
  | [], (e : Chunk.leaf_entry) :: rest ->
    Seq.Cons (Added (e.key, e.value), merge_leaves [] rest)
  | (e : Chunk.leaf_entry) :: rest, [] ->
    Seq.Cons (Removed (e.key, e.value), merge_leaves rest [])
  | (l : Chunk.leaf_entry) :: ls, (r : Chunk.leaf_entry) :: rs ->
    let cmp = String.compare l.key r.key in
    if cmp < 0 then
      Seq.Cons (Removed (l.key, l.value), merge_leaves ls right)
    else if cmp > 0 then
      Seq.Cons (Added (r.key, r.value), merge_leaves left rs)
    else if l.value = r.value then
      merge_leaves ls rs ()
    else
      Seq.Cons (Modified (l.key, l.value, r.value), merge_leaves ls rs)

let emit_all_as tag store h =
  Tree.range store h
  |> Seq.map (fun (k, v) ->
    match tag with
    | `Added -> Added (k, v)
    | `Removed -> Removed (k, v))

let seq_append s1 s2 =
  let rec go s1 s2 () =
    match s1 () with
    | Seq.Nil -> s2 ()
    | Seq.Cons (x, rest) -> Seq.Cons (x, go rest s2)
  in
  go s1 s2

let rec diff_nodes store h1 h2 =
  if Hash.equal h1 h2 then Seq.empty
  else
    let c1 = Chunk.decode (Store.get store h1) in
    let c2 = Chunk.decode (Store.get store h2) in
    match c1, c2 with
    | Chunk.Leaf l1, Chunk.Leaf l2 ->
      merge_leaves l1 l2
    | Chunk.Internal e1, Chunk.Internal e2 ->
      merge_internals store e1 e2
    | _ ->
      (* Mixed types: flatten both to leaves via range *)
      let left = Tree.range store h1 |> List.of_seq in
      let right = Tree.range store h2 |> List.of_seq in
      let to_leaf (k, v) = ({ Chunk.key = k; value = v } : Chunk.leaf_entry) in
      merge_leaves (List.map to_leaf left) (List.map to_leaf right)

and merge_internals store left right =
  let flatten_children store children =
    List.to_seq children
    |> Seq.flat_map (fun (e : Chunk.internal_entry) ->
      Tree.range store e.child
      |> Seq.map (fun (k, v) ->
        ({ Chunk.key = k; value = v } : Chunk.leaf_entry)))
    |> List.of_seq
  in
  (* Collect children from both sides that cover overlapping key ranges.
     We track the max boundary key seen on each side (lmax/rmax) and keep
     advancing the side with the smaller max boundary until they align
     or both are exhausted. *)
  let rec collect lmax rmax lacc racc lrest rrest =
    if lacc <> [] && racc <> [] && lmax = rmax then
      (* Boundaries aligned *)
      (List.rev lacc, List.rev racc, lrest, rrest)
    else
      (* Advance the side with the smaller max boundary *)
      let advance_left =
        match lrest with
        | [] -> false
        | _ -> rmax = "" || lmax < rmax || (lmax = rmax && racc = [])
      in
      let advance_right =
        (not advance_left) &&
        match rrest with
        | [] -> false
        | _ -> true
      in
      if advance_left then
        match lrest with
        | (lh : Chunk.internal_entry) :: lt ->
          collect lh.key rmax (lh :: lacc) racc lt rrest
        | [] -> assert false
      else if advance_right then
        match rrest with
        | (rh : Chunk.internal_entry) :: rt ->
          collect lmax rh.key lacc (rh :: racc) lrest rt
        | [] -> assert false
      else
        (* Both exhausted or nothing more to advance *)
        (List.rev lacc, List.rev racc, lrest, rrest)
  in
  let rec go left right () =
    match left, right with
    | [], [] -> Seq.Nil
    | [], (e : Chunk.internal_entry) :: rest ->
      let added = emit_all_as `Added store e.child in
      seq_append added (go [] rest) ()
    | (e : Chunk.internal_entry) :: rest, [] ->
      let removed = emit_all_as `Removed store e.child in
      seq_append removed (go rest []) ()
    | (l : Chunk.internal_entry) :: ls, (r : Chunk.internal_entry) :: rs ->
      if String.compare l.key r.key = 0 then
        let child_diff = diff_nodes store l.child r.child in
        seq_append child_diff (go ls rs) ()
      else begin
        let lacc, racc, lrest, rrest =
          collect "" "" [] [] (l :: ls) (r :: rs)
        in
        let left_leaves = flatten_children store lacc in
        let right_leaves = flatten_children store racc in
        let diff_seq = merge_leaves left_leaves right_leaves in
        seq_append diff_seq (go lrest rrest) ()
      end
  in
  go left right

let diff store ~from ~to_ =
  if Hash.equal from to_ then Seq.empty
  else diff_nodes store from to_
