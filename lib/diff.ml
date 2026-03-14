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
      let cmp = String.compare l.key r.key in
      if cmp < 0 then
        let removed = emit_all_as `Removed store l.child in
        seq_append removed (go ls right) ()
      else if cmp > 0 then
        let added = emit_all_as `Added store r.child in
        seq_append added (go left rs) ()
      else
        let child_diff = diff_nodes store l.child r.child in
        seq_append child_diff (go ls rs) ()
  in
  go left right

let diff store ~from ~to_ =
  if Hash.equal from to_ then Seq.empty
  else diff_nodes store from to_
