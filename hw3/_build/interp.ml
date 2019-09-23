(* 
Mathew Seedhom
CS 496
I pledge my honor that I have abided by the Stevens Honor System.
*)

open Ast
open Ds

let rec eval (en:env) (e:expr):exp_val =
  match e with
  | Int n           -> NumVal n
  | Var x           -> lookup en x
  | Let(x, e1, e2)  ->
    let v1 = eval en e1  in
    eval (extend_env en x v1) e2
  | IsZero(e1)      ->
    let v1 = eval en e1  in
    let n1 = numVal_to_num v1 in
    BoolVal (n1 = 0)
  | ITE(e1, e2, e3) ->
    let v1 = eval en e1  in
    let b1 = boolVal_to_bool v1 in
    if b1 then eval en e2 else eval en e3
  | Sub(e1, e2)     ->
    let v1 = eval en e1 in
    let v2 = eval en e2  in
    NumVal ((numVal_to_num v1) - (numVal_to_num v2))
  | Add(e1, e2)     -> 
    let v1 = eval en e1 in
    let v2 = eval en e2  in
    NumVal ((numVal_to_num v1) + (numVal_to_num v2))
  | Div(e1, e2)     -> 
    let v1 = eval en e1 in
    let v2 = eval en e2  in
    NumVal ((numVal_to_num v1) / (numVal_to_num v2))
  | Mul(e1, e2)     -> 
    let v1 = eval en e1 in
    let v2 = eval en e2  in
    NumVal ((numVal_to_num v1) * (numVal_to_num v2))
  | Abs(e1)         ->
    let v1 = eval en e1 in
    NumVal(abs (numVal_to_num v1))
  | Cons(e1, e2)    -> 
    let v1 = eval en e1 in
    let v2 = eval en e2 in
    (match v2 with
    | ListVal(x) -> 
      ListVal(v1 :: (listVal_to_list v2))
    | _ -> failwith("Thats not a list!"))
  | Hd(e1)          -> 
    let v1 = eval en e1 in
    (match v1 with 
    | ListVal(x :: xs) -> x
    | ListVal([]) -> failwith("Thats an Empty List!")
    | _ -> failwith("Thats not even a List!"))
  | Tl(e1)          -> 
    (match e1 with 
    | Cons(y, x) -> eval en x
    | EmptyList -> failwith("Thats an Empty List!")
    | _ -> failwith("Thats not even a List!"))
  | Empty(e1)       -> 
    let v1 = eval en e1 in 
    (match v1 with
    | ListVal([]) -> BoolVal(true)
    | TreeVal(Empty) -> BoolVal(true)
    | _ -> BoolVal(false))
  | EmptyList       -> ListVal([])
  | EmptyTree       -> TreeVal(Empty)
  | Node(e1,lt,rt)  ->
    let ltree = treeVal_to_tree (eval en lt) in 
    let rtree = treeVal_to_tree (eval en rt) in 
    let root = eval en e1 in
    TreeVal(Node(root, ltree, rtree))
  | CaseT(target,emptycase,id_e,id_lt,id_rt,nodecase) -> 
    let t = eval en target in
    (match t with 
      | TreeVal(x) ->
        (match x with
        | Empty -> eval en emptycase
        | Node(root, lt, rt) -> 
          let en1 = (extend_env en id_e root) in 
          let en2 = (extend_env en1 id_lt (TreeVal(lt))) in
          let en3 = (extend_env en2 id_rt (TreeVal(rt))) in 
          eval en3 nodecase)
      | _ -> failwith("Target is not a Tree!"))


(***********************************************************************)
(* Everything above this is essentially the same as we saw in lecture. *)
(***********************************************************************)

(* Parse a string into an ast *)
let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(* Interpret an expression *)
let interp (e:string):exp_val =
  e |> parse |> eval (empty_env ())
