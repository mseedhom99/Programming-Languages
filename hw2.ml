(* 
  Mathew Seedhom
  CS 496B
  I pledge my honor that I have abided by the Stevens Honor System
*)

type dtree = Leaf of int | Node of char * dtree * dtree

let tLeft = Node('w', Node('x', Leaf(2), Leaf(5)), Leaf(8))
let tRight = Node('w', Node('x', Leaf(2), Leaf(5)), Node('y', Leaf(7), Leaf(5)))

let rec dTree_height : dtree -> int = fun tree ->
  (* Returns the height of the given tree. *)
  match tree with
  | Leaf(x) -> 0
  | Node(x, y, z) ->
    let yh = dTree_height y in
    let zh = dTree_height z in
    if yh > zh
    then 1 + yh
    else 1+zh

let rec dTree_size : dtree -> int = fun tree ->
  (* Returns the size of the given tree. *)
  match tree with
  | Leaf(x) -> 1
  | Node(x, y, z) -> 1 + (dTree_size y) + (dTree_size z)

let rec dTree_pathsh : dtree -> int list -> int list list = fun tree path->
  (* Helper function for dTree_paths *)
  match tree with
  | Leaf(x) -> [path]
  | Node(x, y, z) -> (dTree_pathsh y (path @ [0])) @ (dTree_pathsh z (path @ [1]))

let rec dTree_paths : dtree -> int list list = fun tree -> 
  (* Returns all possible paths to leaves on the given tree. *)  dTree_pathsh tree []

let rec dTree_is_perfect : dtree -> bool = fun tree ->
  (* Returns if the given tree is a perfect tree. *)
  match tree with
  | Leaf(x) -> true
  | Node(x, y, z) ->
    if dTree_height y = dTree_height z
    then dTree_is_perfect y && dTree_is_perfect z
    else false

let rec dTree_map : (char -> char) -> (int -> int) -> dtree -> dtree = fun f g tree ->
  (* Returns tree with mapped vertex values of given tree. *)
  match tree with
  | Leaf(x) -> Leaf((g x))
  | Node(x, y, z) -> Node( (f x), dTree_map f g y, dTree_map f g z)

let rec list_to_tree : char list -> dtree = fun lst ->
  (* Returns a tree where each node on the nth level corresponds to the nth char in the given list. *)
  match lst with
  | [] -> Leaf(0)
  | x :: xs -> Node(x, list_to_tree xs, list_to_tree xs)

let rec keep : int -> (int list * int) list ->  (int list * int) list= fun n lst ->
  (* Returns list of all (int list * int) where the first item in the int list matches n. *)
  match lst with
  | [] -> []
  | x :: xs -> 
    match (fst x) with
    | [] -> failwith("Impossible!")
    | y :: ys ->
      if y = n
      then (ys, snd x) :: keep n xs
      else keep n xs

let rec replace_leaf_at : dtree -> (int list * int) list -> dtree = fun tree lst ->
  (* Returns a tree where each leaf is redefined as specified. *)
  match tree,lst with
  | Leaf(x), z::zs -> Leaf(snd z)
  | Node(x, y, z),_-> Node(x, replace_leaf_at y (keep 0 lst), replace_leaf_at z (keep 1 lst))
  | Leaf(x), [] -> failwith("Bad input!")

let rec bf_to_dTree : (char list * (int list * int) list) -> dtree = fun pair -> 
  (* Returns an equivalent tree-encoding of a pair-encoding. *)
  replace_leaf_at (list_to_tree (fst pair)) (snd pair)