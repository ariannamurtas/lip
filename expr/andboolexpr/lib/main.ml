open Ast

let rec string_of_boolexpr = function
    True -> "True"
  | False -> "False"
  | If(e0,e1,e2) -> "If(" ^ (string_of_boolexpr e0) ^ "," ^ (string_of_boolexpr e1) ^ "," ^ (string_of_boolexpr e2) ^ ")"
  | Not(e0) -> "Not" ^ (string_of_boolexpr e0)
  | And(e0,e1) -> (string_of_boolexpr e0) ^ "and" ^ (string_of_boolexpr e1)
  | Or(e0,e1) -> (string_of_boolexpr e0) ^ "or" ^ (string_of_boolexpr e1)



let parse (s : string) : boolExpr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
exception NoRuleApplies

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
  | _ -> raise NoRuleApplies

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]

(* big step *)
let rec eval = function
    True -> true
  | False -> false
  | If(e0,e1,e2) -> if eval e0 then eval e1 else eval e2
  | Not(e0) -> not (eval e0)
  | And(e0,e1) -> eval e0 && eval e1
  | Or(e0,e1) -> eval e0 || eval e1
