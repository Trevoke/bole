let build ?(target_size = 64) store pairs =
  (* --- Level 0: Leaf chunks --- *)
  let chunker = Chunker.create ~target_size ~level:0 in
  let current = ref [] in
  let parents = ref [] in
  let prev_key = ref None in

  let flush_leaf () =
    match !current with
    | [] -> ()
    | entries ->
      let entries = List.rev entries in
      let last_key = (List.hd (List.rev entries) : Chunk.leaf_entry).key in
      let data = Chunk.encode (Chunk.Leaf entries) in
      let h = Store.put store data in
      parents := (last_key, h) :: !parents;
      current := []
  in

  Seq.iter (fun (key, value) ->
    (match !prev_key with
     | Some pk when String.compare key pk < 0 ->
       invalid_arg "Tree.build: keys not in sorted order"
     | _ -> ());
    prev_key := Some key;
    current := ({ Chunk.key; value } : Chunk.leaf_entry) :: !current;
    if Chunker.feed chunker ~key then begin
      flush_leaf ();
      Chunker.reset chunker
    end
  ) pairs;

  flush_leaf ();

  match List.rev !parents with
  | [] ->
    (* Empty input *)
    Store.put store (Chunk.encode (Chunk.Leaf []))
  | [(_k, h)] -> h
  | _many ->
    (* Internal levels — next task *)
    assert false
