(* 
  Mathew Seedhom
  CS 496B
  I pledge my honor that I have abided by the Stevens Honor System

Encodings
  0 -> Pen down
  1 -> Pen up
  2 -> Move North
  3 -> Move East
  4 -> Move South
  5 -> Move West
*)

type program = int list

let rec remove_all : 'a -> 'a list -> 'a list = fun el stuff ->
  (* Removes all occurrences of an element from a list *)
  match stuff with
  | [] -> []
  | x::xs ->
    if el=x
    then remove_all el xs
    else x :: remove_all el xs

let rec remove_repeats : 'a list -> 'a list = fun stuff->
  (* Removes repeats of all elements in the list *)
  match stuff with
  | [] -> []
  | x::xs -> x :: remove_repeats (remove_all x xs)

let rec draw : int*int -> int list -> int -> (int*int) list = fun start stuff curr ->
  (* Records all coordinates drawn when the pen is down *)
  match start,stuff,curr with
  | _,[],_ -> []
  | (i,j),x::xs,1 ->
    if x = 0
    then draw (i,j) (x :: xs) 0
    else if x=2
    then draw (i,j+1) xs 1
    else if x=3
    then draw (i+1,j) xs 1
    else if x=4
    then draw (i,j-1) xs 1
    else if x=5
    then draw (i-1,j) xs 1
    else draw (i,j) xs 1
  | (i,j),x::xs,0 ->
    if x = 0
    then (i,j) :: draw (i,j) xs 0
    else if x=1
    then draw (i,j) xs 1
    else if x=2
    then (i,j+1) :: draw (i,j+1) xs 0
    else if x=3
    then (i+1,j) :: draw (i+1,j) xs 0
    else if x=4
    then (i,j-1) :: draw (i,j-1) xs 0
    else if x=5
    then (i-1,j) :: draw (i-1,j) xs 0
    else draw (i,j) xs 0
  | _,_,_ -> []

let rec colored : int*int -> program -> (int*int) list = fun start stuff ->
  (* Records all coordinates drawn on with no repeats *)
  match stuff with
  | [] -> []
  | x::xs -> remove_repeats (draw start (x :: xs) 1)

let rec equivalence : (int*int) list -> (int*int) list -> bool = fun a b ->
  (* Checks if two programs color the same coordinates *)
  match a,b with
  | [],[] -> true
  | x::xs,[] -> false
  | [],y::ys -> false
  | x::xs,y::ys -> equivalence xs (remove_all x (y::ys))

let equivalent : program -> program -> bool = fun a b -> equivalence (colored (0,0) a) (colored (0,0) b)

let rec mirror_image : program -> program = fun a -> 
  (* Mirrors all program instruction directions *)
  match a with
  | [] -> []
  | x::xs ->
    if x = 0 || x=1
    then x :: mirror_image xs
    else if x = 2
    then 4 :: mirror_image xs
    else if x = 3
    then 5 :: mirror_image xs
    else if x = 4
    then 2 :: mirror_image xs
    else if x = 5
    then 3 :: mirror_image xs
    else mirror_image xs


let rec rotate_90 : program -> program = fun a ->
  (* Rotates all program instructions by 90 degress *)
  match a with
  | [] -> []
  | x::xs -> 
    if x = 0 || x = 1
    then x :: rotate_90 xs
    else if x = 2
    then 3 :: rotate_90 xs
    else if x = 3
    then 4 :: rotate_90 xs
    else if x = 4
    then 5 :: rotate_90 xs
    else if x = 5
    then 2 :: rotate_90 xs
    else rotate_90 xs


let rec repeat : int -> 'a -> 'a list = fun n x ->
  (* Repeats an element for a specified amount of times in a list *)
  match n with
  | 0 -> []
  | n -> x :: (repeat (n-1) x)


let rec pantograph : program -> int -> program = fun p n ->
  (* Gives a program that scales the original programs drawing by a given factor *)
  match p,n with
  | [],n -> []
  | _,0 -> []
  | x::xs,n -> 
    if x = 0 || x = 1
    then x :: (pantograph xs n)
    else if x > 1 || x < 6
    then (repeat n x) @ (pantograph xs n)
    else pantograph xs n

let rec count_f: 'a list -> 'a -> int = fun p e ->
  (* Counts how many times an elements repeats itself in the beginning of a list *)
  match p with 
  | [] -> 0
  | x::xs ->
    if x = e
    then 1 + (count_f xs e)
    else 0

let rec remove : 'a list -> int -> 'a list = fun p n ->
  (* Removes the first n elements of a list *)
  match p,n with
  | [],_ -> []
  | x::xs,0 -> x::xs
  | x::xs,n -> remove xs (n-1)


let rec compress : program -> (int*int) list = fun p ->
  (* Compresses the instructions given by the program *)
  match p with
  | [] -> []
  | x::xs -> 
    if x > -1 || x < 6
    then (x,(1 + count_f xs x)) :: compress (remove xs (count_f xs x))
    else compress xs

let first : int*int -> int = fun (i, j) -> i
(* Returns the first element in a tuple *)

let second : int*int -> int = fun (i, j) -> j
(* Returns the second element in a tuple *)

let rec decompress : (int*int) list -> program = fun p ->
  (* Decompresses a compressed version of a program *)
  match p with
  | [] -> []
  | x::xs -> 
    if first x > -1 && first x < 6 && second x > 0 
    then (repeat (second x) (first x)) @ (decompress xs)
    else decompress xs
