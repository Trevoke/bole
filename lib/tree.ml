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
      let last_key = (List.hd entries : Chunk.leaf_entry).key in
      let entries = List.rev entries in
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
          let last_key = (List.hd ents : Chunk.internal_entry).key in
          let ents = List.rev ents in
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
         failwith "Tree.build: internal error: empty level"
       | _ ->
         entries := next;
         level := !level + 1)
    done;
    !result

let find store root key =
  let rec descend h =
    let data = Store.get store h in
    let chunk = Chunk.decode data in
    match chunk with
    | Chunk.Leaf entries ->
      let rec scan = function
        | [] -> None
        | (e : Chunk.leaf_entry) :: rest ->
          if e.key = key then Some e.value
          else scan rest
      in
      scan entries
    | Chunk.Internal entries ->
      let rec find_child = function
        | [] -> None
        | (e : Chunk.internal_entry) :: _ when e.key >= key ->
          descend e.child
        | _ :: rest -> find_child rest
      in
      find_child entries
  in
  descend root

type path_frame = {
  entries : Chunk.internal_entry array;
  index : int;
}

let range ?start_key ?end_key store root =
  (* Descend to the leftmost relevant leaf, recording the path *)
  let rec descend h path =
    let data = Store.get store h in
    let chunk = Chunk.decode data in
    match chunk with
    | Chunk.Leaf entries -> (entries, path)
    | Chunk.Internal entries ->
      let arr = Array.of_list entries in
      let idx = match start_key with
        | None -> 0
        | Some sk ->
          let rec find_idx i =
            if i >= Array.length arr then Array.length arr - 1
            else if arr.(i).Chunk.key >= sk then i
            else find_idx (i + 1)
          in
          find_idx 0
      in
      let frame = { entries = arr; index = idx } in
      descend arr.(idx).Chunk.child (frame :: path)
  in

  (* Advance to next leaf via the path *)
  let rec next_leaf path =
    match path with
    | [] -> None
    | frame :: rest ->
      let next_idx = frame.index + 1 in
      if next_idx >= Array.length frame.entries then
        next_leaf rest
      else
        let frame = { frame with index = next_idx } in
        let child = frame.entries.(next_idx).Chunk.child in
        let rec descend_left h p =
          let data = Store.get store h in
          let chunk = Chunk.decode data in
          match chunk with
          | Chunk.Leaf entries -> Some (entries, p)
          | Chunk.Internal entries ->
            let arr = Array.of_list entries in
            let f = { entries = arr; index = 0 } in
            descend_left arr.(0).Chunk.child (f :: p)
        in
        descend_left child (frame :: rest)
  in

  (* Find starting position in leaf *)
  let start_pos entries =
    match start_key with
    | None -> 0
    | Some sk ->
      let rec find i = function
        | [] -> i
        | (e : Chunk.leaf_entry) :: rest ->
          if e.key >= sk then i
          else find (i + 1) rest
      in
      find 0 entries
  in

  (* Check end bound *)
  let past_end key =
    match end_key with
    | None -> false
    | Some ek -> key >= ek
  in

  (* Build the Seq.t *)
  let initial_leaf, initial_path = descend root [] in
  let pos = start_pos initial_leaf in
  let state = ref (Some (initial_leaf, pos, initial_path)) in

  let rec seq () =
    match !state with
    | None -> Seq.Nil
    | Some (entries, pos, path) ->
      if pos >= List.length entries then begin
        match next_leaf path with
        | None ->
          state := None;
          Seq.Nil
        | Some (new_entries, new_path) ->
          state := Some (new_entries, 0, new_path);
          seq ()
      end else
        let e = List.nth entries pos in
        if past_end e.Chunk.key then begin
          state := None;
          Seq.Nil
        end else begin
          state := Some (entries, pos + 1, path);
          Seq.Cons ((e.Chunk.key, e.value), seq)
        end
  in
  seq
