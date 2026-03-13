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

  let parent_list = List.rev !parents in

  match parent_list with
  | [] ->
    (* Empty input *)
    Store.put store (Chunk.encode (Chunk.Leaf []))
  | [(_k, h)] -> h
  | _ ->
    (* --- Level 1+: Internal nodes --- *)
    let level = ref 1 in
    let entries = ref parent_list in
    let result = ref (snd (List.hd parent_list)) in
    let continue = ref true in
    while !continue do
      let chunker = Chunker.create ~target_size ~level:!level in
      let current = ref [] in
      let next_parents = ref [] in

      let flush_internal () =
        match !current with
        | [] -> ()
        | ents ->
          let ents = List.rev ents in
          let last_key = (List.hd (List.rev ents) : Chunk.internal_entry).key in
          let data = Chunk.encode (Chunk.Internal ents) in
          let h = Store.put store data in
          next_parents := (last_key, h) :: !next_parents;
          current := []
      in

      List.iter (fun (key, child) ->
        current := ({ Chunk.key; child } : Chunk.internal_entry) :: !current;
        if Chunker.feed chunker ~key then begin
          flush_internal ();
          Chunker.reset chunker
        end
      ) !entries;

      flush_internal ();

      let next = List.rev !next_parents in
      (match next with
       | [(_k, h)] ->
         result := h;
         continue := false
       | [] ->
         continue := false
       | _ ->
         entries := next;
         level := !level + 1)
    done;
    !result
