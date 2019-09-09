let remove ~idx l =
  let rec aux ~left ~right ~i =
    match right with
    | _::tl when i = idx -> List.rev_append left tl
    | hd::tl -> aux ~left:(hd::left) ~right:tl ~i:(i + 1)
    | [] -> l
  in
  aux ~left:[] ~right:l ~i:0

let rec permutations = function
  | [] -> [[]]
  | [elm] -> [[elm]]
  | l ->
    List.mapi
      (fun idx elm -> List.map (List.cons elm) (permutations @@ remove ~idx l))
      l
  |> List.flatten
(* 
let split_list l ~length =
  let rec aux ~left ~right =
    if List.length left >= length then
      left, right
    else
      match right with
      | [] -> left, right
      | x :: xs -> aux ~left:(x :: left) ~right
  in
  let left, right = aux ~left:[] ~right:l in
  List.rev left, right *)

(* [] -> []
[a] -> [a]; [_; a]; [_; _; a]; [_; _; _; a]
[a; b] -> [a; b]; [_; a; b]; [a; _; b]; [_; _; a; b]; [_; a; _; b]; [a; _; _; b]
[a; b; c] -> [a; b; c]; [_; a; b; c]; [a; _; b; c]; [a; b; _; c] *)
let rec permutations_with_padding ~length ~padding = function
| [] -> [[]]
| [elm] ->
  let rec aux ~acc ~longest =
    if List.length longest = length then
      acc
    else
      aux ~acc:(longest :: acc) ~longest:(padding :: longest)
  in
  aux ~acc:[] ~longest:[elm]
| x :: xs ->
  let perms = permutations_with_padding xs ~length:(length - 1) ~padding in
  List.map (fun perm ->
    let length_allowed = length - List.length perm in
    let prefixes = permutations_with_padding ~length:length_allowed ~padding [x] in
    List.map (fun prefix -> prefix @ perm) prefixes
  ) perms
  |> List.concat
