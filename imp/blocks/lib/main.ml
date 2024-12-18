open Ast
open Types


let parse (s : string) : cmd =  
  let lexbuf = Lexing.from_string s in  
  let ast = Parser.prog Lexer.read lexbuf in  
  ast 


  
(* BIG STEP SEMANTIC, restituisce senza passaggi intermedi un risultato *)
 let rec eval_expr (state : state) (expr : expr) : memval =
  match expr with
  | True -> Bool true
  | False -> Bool false

  | And (e0, e1) -> (
      match (eval_expr state e0, eval_expr state e1) with
      | Bool b1, Bool b2 -> Bool (b1 && b2)
      | _ -> failwith "I parametri di And devono essere booleani")

  | Or (e0, e1) -> (
      match (eval_expr state e0, eval_expr state e1) with
      | Bool b1, Bool b2 -> Bool (b1 || b2)
      | _ -> failwith "I parametri di Or devono essere booleani")

  | Not e0 -> (
      match eval_expr state e0 with
      | Bool b -> Bool (not b)
      | _ -> failwith "Il parametro di Not deve essere booleano")

  | Add (e0, e1) -> (
      match (eval_expr state e0, eval_expr state e1) with
      | Int n1, Int n2 -> Int (n1 + n2)
      | _ -> failwith "I parametri di Add devono essere numeri")

  | Sub (e0, e1) -> (
      match (eval_expr state e0, eval_expr state e1) with
      | Int n1, Int n2 -> Int (n1 - n2)
      | _ -> failwith "I parametri di Sub devono essere numeri")

  | Mul (e0, e1) -> (
      match (eval_expr state e0, eval_expr state e1) with
      | Int n1, Int n2 -> Int (n1 * n2)
      | _ -> failwith "I parametri di Mul devono essere numeri")

  | Eq (a, b) -> (
      match (eval_expr state a, eval_expr state b) with
      | Bool a, Bool b -> Bool (a = b)
      | Int a, Int b -> Bool (a = b)
      | _ -> failwith "I valori di Eq devono essere dello stesso tipo")

  | Leq (a, b) -> (
      match (eval_expr state a, eval_expr state b) with
      | Int a, Int b -> Bool (a <= b)
      | _ -> failwith "I valori di Leq devono essere entrambi numerici")

  | Const num -> Int num

  | Var var -> (
      let env = topenv state in
      match env var with
      | BVar loc
      | IVar loc -> (getmem state) loc
    )
  

  let eval_decl (state : state) (decl_list : decl list) : state =
    let (env, loc) =
      List.fold_left
        (fun (env, loc) decl ->
           match decl with
           | IntVar var -> (bind_env env var (IVar loc), loc + 1)
           | BoolVar var -> (bind_env env var (BVar loc), loc + 1))
        (topenv state, getloc state)
        decl_list in
    let envstack = getenv state in
    make_state (env :: envstack) (getmem state) loc


let rec trace1 : conf -> conf = function
  | Cmd (Skip, state) -> St state 
  | Cmd (Assign (var,expr), state) -> 
    let (env, mem) = (topenv state, getmem state) in

    let new_mem = match 
    eval_expr state expr with 
    | Int ivar -> (
      match env var with 
      | IVar i -> bind_mem mem i (Int ivar)
      | _ -> failwith "var deve essere int" )
    | Bool bvar -> (
      match env var with 
      | BVar b -> bind_mem mem b (Bool bvar)
      | _ -> failwith "var deve essere bool" ) in 
    
    St (make_state (getenv state) new_mem (getloc state))
  
  | Cmd (Seq (command1,command2), state) -> (
    match trace1 (Cmd (command1,state)) with 
    | Cmd (command1',state') -> Cmd (Seq (command1',command2), state')
    | St state' -> Cmd (command2, state'))

  | Cmd (If (expr,c_then,c_else), state) -> (
    match eval_expr state expr with 
    | Bool b -> if b then Cmd (c_then, state) else Cmd (c_else, state)
    | _ -> failwith "Errore, expr di If vuole un valore booleano" )

  | Cmd (While (expr,command), state) -> (
    match eval_expr state expr with 
    | Bool true -> Cmd (Seq (command, While (expr, command)), state)
    | Bool false -> St state
    | _ -> failwith "Errore, expr di While non è un valore booleano")
  
  | Cmd (Decl (decl_list, command), state) -> (
    let new_state = eval_decl state decl_list in 
    match trace1 (Cmd (command,new_state)) with 
    | St state' -> St (make_state (popenv state') (getmem state') (getloc state'))
    | Cmd (command',state') -> Cmd (Block command', state'))

  | Cmd (Block command, state) -> (
    match trace1 (Cmd (command, state)) with 
    | St state' -> St (make_state (popenv state') (getmem state') (getloc state'))
    | Cmd (command',state') -> Cmd (Block command', state'))
  
  | _ -> raise NoRuleApplies

let trace (n_steps : int) (command : cmd) : conf list =
  let conf0 = Cmd (command, state0) in
  let rec helper n_steps conf =
    if n_steps > 0 then
      try
        let conf1 = trace1 conf in
        conf :: helper (n_steps - 1) conf1
      with NoRuleApplies -> [ conf ]
    else [ conf ]
  in helper n_steps conf0

 