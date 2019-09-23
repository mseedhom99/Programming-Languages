(*
Mathew Seedhom

I pledge my honor that I have abided by the Stevens Honor System.
*)

open Ast

let from_some = function
  | None -> failwith "from_some: None"
  | Some v -> v

(*  ;;;;;;;;;;;;;;;; type environments ;;;;;;;;;;;;;;;; *)

type tenv =
  | EmptyTEnv
  | ExtendTEnv of string*texpr*tenv

let empty_tenv () = EmptyTEnv

let extend_tenv id t tenv = ExtendTEnv(id,t,tenv)


let rec apply_tenv (tenv:tenv) (id:string):texpr option =
  match tenv with
  | EmptyTEnv -> None
  | ExtendTEnv (key,value,tenv1) ->
    if id=key
    then Some value
    else apply_tenv tenv1 id


let init_tenv () =
     extend_tenv "x"  IntType
     @@ extend_tenv "v" IntType
     @@ extend_tenv "i"  IntType
     @@ empty_tenv ()

let rec  string_of_tenv  = function
  | EmptyTEnv -> ""
  | ExtendTEnv(id,v,env) -> "("^id^","^string_of_texpr v^")"^string_of_tenv env



let rec type_of_prog = function
  | AProg e -> type_of_expr (init_tenv ()) e
and
  type_of_expr en = function
  | Int n          -> IntType
  | Var id          ->
    (match apply_tenv en id with
    | None -> failwith @@ "Variable "^id^" undefined"
    | Some texp -> texp)
  | Unit ->
    UnitType
  | ITE(e1, e2, e3)    ->
    let t1 = type_of_expr en e1
    in let t2 = type_of_expr en e2
    in let t3 = type_of_expr en e3
    in if t1=BoolType && t2=t3
    then t2
    else failwith "ITE: Type error"
  | Add(e1, e2) | Mul(e1,e2) | Sub(e1,e2) | Div(e1,e2)    ->
    let t1 = type_of_expr en e1 in
    let t2 = type_of_expr en e2  in
    if t1=IntType && t2=IntType
    then IntType
    else failwith "Add: arguments must be ints"
  | IsZero(e) ->
    let t1 = type_of_expr en e  in
    if t1=IntType
    then BoolType
    else failwith "Zero?: argument must be int"
  | Let(x, e1, e2) ->
    let t1 = type_of_expr en e1
    in type_of_expr (extend_tenv x t1 en) e2
  | Proc(x,ty,e)      ->
    let tc= type_of_expr (extend_tenv x ty en) e
    in FuncType(ty,tc)
  | App(e1,e2)     ->
    let t1 = type_of_expr en e1
    in let t2 = type_of_expr en e2
    in (match t1 with
    | FuncType(td,tcd) when td=t2 -> tcd
    | FuncType(td,tcd) -> failwith "App: argument does not have correct type"
    | _ -> failwith "Checker: App: LHS must be function type")
  | Letrec(tRes,id,param,tParam,body,e) ->
    let t=type_of_expr (extend_tenv param tParam
                          (extend_tenv id (FuncType(tParam,tRes)) en))
        body
    in if t=tRes
    then type_of_expr (extend_tenv id (FuncType(tParam,tRes)) en) e
    else failwith
        "Checker: LetRec: Types of recursive function does not match declaration"
  | Set(id,e) ->
      failwith "EXPLICIT-REFS: Set not a valid operation"
  | BeginEnd(es) ->
    List.fold_left (fun v e -> type_of_expr en e) UnitType es

  (* explicit ref *)
  | NewRef(e) ->
    let etype = type_of_expr en e
    in RefType(etype)
  | DeRef(e) ->
    let etype = type_of_expr en e
    in (match etype with
    | RefType(vtype) -> vtype
    | _ -> failwith "Can only deref a reference")
  | SetRef(e1,e2) ->
    let e2type = type_of_expr en e2
    in let e1type = type_of_expr en e1
    in if RefType(e2type) = e1type
    then UnitType
    else failwith "Attempting to change predetermined type"

  (* pair *)
  | Pair(e1, e2) -> PairType( type_of_expr en e1, type_of_expr en e2)
  | Unpair(id1, id2, def, body) ->
    let defType = type_of_expr en def
    in (match defType with
    | PairType(t1, t2) -> type_of_expr (extend_tenv id2 t2 (extend_tenv id1 t1 en)) body
    | _ -> failwith "Does not match with proper pair typing")

  (* list *)
  | EmptyList(t) -> ListType(t)
  | Cons(he, te) ->
    let teType = type_of_expr en te
    in let heType = type_of_expr en he
    in if teType = ListType(heType)
    then teType
    else failwith "Head Type does not match interior Tail type"
  | Null(e) ->
    let eType = type_of_expr en e
    in (match eType with
    | ListType(_) -> BoolType
    | _ -> failwith "Can only check if lists are null")
  | Hd(e) ->
    let eType = type_of_expr en e
    in (match eType with
    | ListType(v) -> v
    | _ -> failwith "Can only find head of lists")
  | Tl(e) ->
    let eType = type_of_expr en e
    in (match eType with
    | ListType(_) -> eType
    | _ -> failwith "Can only find tail of lists")

  (* tree *)
  | EmptyTree(t) -> TreeType(t)
  | Node(de, le, re) ->
    let deType = type_of_expr en de
    in let leType =  type_of_expr en le
    in let reType =  type_of_expr en re
    in if (leType = TreeType(deType) && reType = TreeType(deType))
    then leType
    else failwith "Inconsistent types of nodes"
  | NullT(t) ->
    let tType = type_of_expr en t
    in (match tType with
    | TreeType(_) -> BoolType
    | _ -> failwith "Can only check if trees are null")
  | GetData(t) ->
    let tType = type_of_expr en t
    in (match tType with
    | TreeType(v) -> v
    | _ -> failwith "Can only get data from nodes")
  | GetLST(t) ->
    let tType = type_of_expr en t
    in (match tType with
    | TreeType(_) -> tType
    | _ -> failwith "Can only get the left subtree of a tree")
  | GetRST(t) ->
    let tType = type_of_expr en t
    in (match tType with
    | TreeType(_) -> tType
    | _ -> failwith "Can only get the right subtree of a tree")


  | Debug ->
    print_string "Environment:\n";
    print_string @@ string_of_tenv en;
    UnitType



let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(* Interpret an expression *)
let chk (e:string) : texpr =
  e |> parse |> type_of_prog

