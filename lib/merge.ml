type change =
  | Put of string * string
  | Delete of string

type conflict = {
  key : string;
  base : string option;
  ours : string option;
  theirs : string option;
}

type result = {
  changes : change list;
  conflicts : conflict list;
}

let diff_entry_key = function
  | Diff.Added (k, _) -> k
  | Diff.Removed (k, _) -> k
  | Diff.Modified (k, _, _) -> k

let diff_entry_equal a b =
  match a, b with
  | Diff.Added (k1, v1), Diff.Added (k2, v2) -> k1 = k2 && v1 = v2
  | Diff.Removed (k1, v1), Diff.Removed (k2, v2) -> k1 = k2 && v1 = v2
  | Diff.Modified (k1, o1, n1), Diff.Modified (k2, o2, n2) ->
    k1 = k2 && o1 = o2 && n1 = n2
  | _ -> false

let change_of_theirs_entry = function
  | Diff.Added (k, v) -> Put (k, v)
  | Diff.Removed (k, _) -> Delete k
  | Diff.Modified (k, _, new_v) -> Put (k, new_v)

let conflict_of_entries ~ours_entry ~theirs_entry =
  let key = diff_entry_key ours_entry in
  let base =
    match ours_entry with
    | Diff.Modified (_, old_v, _) -> Some old_v
    | Diff.Removed (_, old_v) -> Some old_v
    | Diff.Added _ -> None
  in
  let ours =
    match ours_entry with
    | Diff.Added (_, v) | Diff.Modified (_, _, v) -> Some v
    | Diff.Removed _ -> None
  in
  let theirs =
    match theirs_entry with
    | Diff.Added (_, v) | Diff.Modified (_, _, v) -> Some v
    | Diff.Removed _ -> None
  in
  { key; base; ours; theirs }

let three_way ~ours ~theirs =
  let rec go ours theirs changes_acc conflicts_acc =
    match ours (), theirs () with
    | Seq.Nil, Seq.Nil ->
      { changes = List.rev changes_acc; conflicts = List.rev conflicts_acc }
    | Seq.Nil, Seq.Cons (t_entry, theirs_rest) ->
      let change = change_of_theirs_entry t_entry in
      go (fun () -> Seq.Nil) theirs_rest (change :: changes_acc) conflicts_acc
    | Seq.Cons (_, ours_rest), Seq.Nil ->
      go ours_rest (fun () -> Seq.Nil) changes_acc conflicts_acc
    | Seq.Cons (o_entry, ours_rest), Seq.Cons (t_entry, theirs_rest) ->
      let o_key = diff_entry_key o_entry in
      let t_key = diff_entry_key t_entry in
      let cmp = String.compare o_key t_key in
      if cmp < 0 then
        go ours_rest theirs changes_acc conflicts_acc
      else if cmp > 0 then
        let change = change_of_theirs_entry t_entry in
        go ours theirs_rest (change :: changes_acc) conflicts_acc
      else if diff_entry_equal o_entry t_entry then
        go ours_rest theirs_rest changes_acc conflicts_acc
      else
        let conflict = conflict_of_entries ~ours_entry:o_entry ~theirs_entry:t_entry in
        go ours_rest theirs_rest changes_acc (conflict :: conflicts_acc)
  in
  go ours theirs [] []
