let default_target_size = 64

(** Chunk a list of leaf entries, store chunks,
    return (last_key, hash) pairs. *)
let chunk_leaf_entries ~target_size store entries =
  match entries with
  | [] -> []
  | _ ->
    let chunker = Chunker.create ~target_size ~level:0 in
    let current = ref [] in
    let result = ref [] in
    List.iter (fun (e : Chunk.leaf_entry) ->
      current := e :: !current;
      if Chunker.feed chunker ~key:e.key then begin
        let last_key = (List.hd !current).Chunk.key in
        let ents = List.rev !current in
        let data = Chunk.encode (Chunk.Leaf ents) in
        let h = Store.put store data in
        result := (last_key, h) :: !result;
        current := [];
        Chunker.reset chunker
      end
    ) entries;
    (match !current with
     | [] -> ()
     | ents ->
       let last_key = (List.hd ents : Chunk.leaf_entry).key in
       let ents = List.rev ents in
       let data = Chunk.encode (Chunk.Leaf ents) in
       let h = Store.put store data in
       result := (last_key, h) :: !result);
    List.rev !result

(** Chunk a list of internal entries at the given level, store chunks,
    return (last_key, hash) pairs. *)
let chunk_internal_entries ~target_size ~level store entries =
  match entries with
  | [] -> []
  | _ ->
    let chunker = Chunker.create ~target_size ~level in
    let current = ref [] in
    let result = ref [] in
    List.iter (fun (e : Chunk.internal_entry) ->
      current := e :: !current;
      if Chunker.feed chunker ~key:e.key then begin
        let last_key = (List.hd !current).Chunk.key in
        let ents = List.rev !current in
        let data = Chunk.encode (Chunk.Internal ents) in
        let h = Store.put store data in
        result := (last_key, h) :: !result;
        current := [];
        Chunker.reset chunker
      end
    ) entries;
    (match !current with
     | [] -> ()
     | ents ->
       let last_key = (List.hd ents : Chunk.internal_entry).key in
       let ents = List.rev ents in
       let data = Chunk.encode (Chunk.Internal ents) in
       let h = Store.put store data in
       result := (last_key, h) :: !result);
    List.rev !result

(** Build internal levels from parent entries until a single root.
    Returns the root hash. *)
let build_upper_levels ~target_size store parent_list =
  match parent_list with
  | [] -> Store.put store (Chunk.encode (Chunk.Leaf []))
  | [(_k, h)] -> h
  | _ ->
    let level = ref 1 in
    let entries = ref parent_list in
    let result = ref (snd (List.hd parent_list)) in
    let continue = ref true in
    while !continue do
      let pairs = List.map (fun (key, child) ->
        ({ Chunk.key; child } : Chunk.internal_entry)
      ) !entries in
      let next = chunk_internal_entries ~target_size ~level:!level store pairs in
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

let build ?(target_size = default_target_size) store pairs =
  let prev_key = ref None in
  let all_entries = ref [] in

  Seq.iter (fun (key, value) ->
    (match !prev_key with
     | Some pk when String.compare key pk < 0 ->
       invalid_arg "Tree.build: keys not in sorted order"
     | _ -> ());
    prev_key := Some key;
    all_entries := ({ Chunk.key; value } : Chunk.leaf_entry) :: !all_entries
  ) pairs;

  let entries = List.rev !all_entries in
  let parent_list = chunk_leaf_entries ~target_size store entries in
  build_upper_levels ~target_size store parent_list

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

type mut_frame = {
  internal_entries : Chunk.internal_entry list;
  child_index : int;
}

let mutate ~target_size store root key modify_leaf =
  let rec descend h path =
    let data = Store.get store h in
    let chunk = Chunk.decode data in
    match chunk with
    | Chunk.Leaf entries -> (entries, path)
    | Chunk.Internal entries ->
      let rec find_idx i = function
        | [] -> i - 1
        | (e : Chunk.internal_entry) :: _ when e.key >= key -> i
        | _ :: rest -> find_idx (i + 1) rest
      in
      let idx = max 0 (find_idx 0 entries) in
      let child = (List.nth entries idx).Chunk.child in
      let frame = { internal_entries = entries; child_index = idx } in
      descend child (frame :: path)
  in

  let leaf_entries, path = descend root [] in
  let new_entries = modify_leaf leaf_entries in

  let parent_entries = chunk_leaf_entries ~target_size store new_entries in

  let rec propagate path entries level =
    match path with
    | [] -> build_upper_levels ~target_size store entries
    | frame :: rest ->
      let old = frame.internal_entries in
      let before = List.filteri (fun i _ -> i < frame.child_index) old in
      let after = List.filteri (fun i _ -> i > frame.child_index) old in
      let new_internal =
        before
        @ List.map (fun (k, h) ->
            ({ Chunk.key = k; child = h } : Chunk.internal_entry)) entries
        @ after
      in
      let new_pairs = chunk_internal_entries ~target_size ~level store
        new_internal in
      propagate rest new_pairs (level + 1)
  in
  propagate path parent_entries 1

let put ?(target_size = default_target_size) store root key value =
  mutate ~target_size store root key (fun leaf_entries ->
    let rec insert_sorted acc = function
      | [] -> List.rev (({ Chunk.key; value } : Chunk.leaf_entry) :: acc)
      | (e : Chunk.leaf_entry) :: rest when e.key = key ->
        List.rev_append (({ Chunk.key; value } : Chunk.leaf_entry) :: acc) rest
      | (e : Chunk.leaf_entry) :: rest when e.key > key ->
        List.rev_append (e :: ({ Chunk.key; value } : Chunk.leaf_entry) :: acc) rest
      | e :: rest -> insert_sorted (e :: acc) rest
    in
    insert_sorted [] leaf_entries)

let delete ?(target_size = default_target_size) store root key =
  mutate ~target_size store root key (fun leaf_entries ->
    let rec remove found acc = function
      | [] ->
        if not found then raise Not_found
        else List.rev acc
      | (e : Chunk.leaf_entry) :: rest when e.key = key ->
        remove true acc rest
      | e :: rest -> remove found (e :: acc) rest
    in
    remove false [] leaf_entries)
