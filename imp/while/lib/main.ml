open Ast
exception TypeError of string

type exprval = Bool of bool | Nat of int
type exprtype = BoolT | NatT 

let string_of_type = function
    NatT -> "Nat"
    | BoolT -> "Bool"

let string_of_val = function
    Bool true -> "True"
  | Bool false -> "False"
  | Nat n -> string_of_int n

let rec string_of_expr = function
    True -> "True"
  | False -> "False"
  | If(e0,e1,e2) -> "If(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Not(e0) -> "Not" ^ (string_of_expr e0)
  | And(e0,e1) -> (string_of_expr e0) ^ "and" ^ (string_of_expr e1)
  | Or(e0,e1) -> (string_of_expr e0) ^ "or" ^ (string_of_expr e1)
  | Zero -> "0"
  | Succ(e0) -> "succ" ^ (string_of_expr e0)
  | Pred(e0)-> "pred" ^ (string_of_expr e0)
  | IsZero(e0)-> "iszero" ^ (string_of_expr e0)



let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
exception NoRuleApplies

let rec is_nv nv = match nv with
  | Zero -> true
  | Succ e1 -> is_nv e1
  | _-> false

(* small step *)
let rec trace1 = function
    If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(e0,e1,e2) -> If (trace1 e0,e1,e2)
  | Not(False) -> True
  | Not(True) -> False
  | Not(e0) -> Not(trace1 e0)
  | And(True, True) -> True
  | And(False,_) -> False
  | And(_, False) -> False
  | And(e0, e1) -> And(trace1 e0, trace1 e1)
  | Or(False, False) -> False
  | Or(True,_) -> True
  | Or(_,True) -> True
  | Or(e0,e1) -> Or(trace1 e0, trace1 e1)
  | Succ(e0) -> Succ(trace1 e0)
  | Pred( Succ(e0)) -> e0
  | Pred(e0) -> Pred(trace1 e0)
  | IsZero Zero -> True
  | IsZero(Succ _) -> False
  | IsZero(e0) -> IsZero(trace1 e0)
  | _ -> raise NoRuleApplies

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]

(* big step *)
let rec eval = function
    True -> Bool true

  | False -> Bool false

  | If(e0,e1,e2) -> 
    (match eval e0 with
    | Bool true -> eval e1
    | Bool false -> eval e2
    | _-> failwith "Not Bool")

  | Not(e0) -> 
    (match eval e0 with
    | Bool e0 -> Bool (not e0)
    | _-> failwith "Not Bool")

  | And(e0,e1) ->
    (match eval e0, eval e1 with
    | Bool e0, Bool e1 -> Bool(e0 && e1)
    | _-> failwith "Not Bool")

  | Or(e0,e1) -> 
    (match eval e0, eval e1 with
    | Bool e0, Bool e1 -> Bool(e0 || e1)
    |_-> failwith "Not Bool")

  | Zero -> Nat 0

  | Succ(e0) -> 
    (match eval e0 with
    | Nat e0 -> Nat(e0+1)
    | _-> failwith "Not Nat")

  | Pred(e0)->
    (match eval e0 with
    | Nat e0 -> if e0>0 then Nat(e0 - 1) else failwith "Nat is zero"
    | _-> failwith "Not Nat")

  | IsZero(e0) ->
    (match eval e0 with
    | Nat 0 -> Bool true
    | Nat n -> if n>0 then Bool false else failwith "Cannot solve"
    | _-> failwith "Not Nat")

  
let rec typecheck = function 
  True -> BoolT

  | False -> BoolT

  | Zero -> NatT

  | If(e0, e1, e2) ->
    (match typecheck e0, typecheck e1, typecheck e2 with
    | NatT,_,_-> raise (TypeError "Type Bool was expected")
    | BoolT, NatT, NatT -> NatT
    | BoolT, BoolT, BoolT -> BoolT
    | _-> raise(TypeError "Excepted bool"))

  | Not(e0)->
    (match typecheck e0 with
    | BoolT -> BoolT
    | _-> raise(TypeError "Excepted bool"))

  | And(e0, e1)->
    (match typecheck e0, typecheck e1 with
    | BoolT, BoolT -> BoolT
    | _-> raise(TypeError ""))

  | Or(e0,e1)->
    (match typecheck e0, typecheck e1 with
    | BoolT, BoolT -> BoolT
    | _-> raise(TypeError "Type Bool was expected"))

  | Succ(e0)->
    (match typecheck e0 with
    | NatT -> NatT
    | _-> raise(TypeError "Type Bool was expected"))

  | Pred(e0)->
    (match typecheck e0 with
    | NatT -> NatT
    | _-> raise(TypeError "Type Bool was expected"))

  | IsZero(e0)->
    (match typecheck e0 with
    | NatT -> BoolT
    | _-> raise(TypeError "Type Bool was expected"))
  