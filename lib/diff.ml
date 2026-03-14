type entry =
  | Added of string * string
  | Removed of string * string
  | Modified of string * string * string

let diff store ~from ~to_ =
  if Hash.equal from to_ then Seq.empty
  else
    (* Placeholder: flatten both trees and merge-join *)
    let _ = store in
    let _ = from in
    let _ = to_ in
    Seq.empty
