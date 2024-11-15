open BoolexprLib.Main

(* let test_eval expr exp_result =
  (expr |> parse |> eval) = exp_result 

### Unit tests for task 4 

let%test "test_eval_1" = failwith "TODO" 

### Unit tests for task 5 

let%test "test_trace1_1" = failwith "TODO" *)

let%test "test_parse1" = parse "true" |> eval = true;;
let%test "test_parse2" = parse "if true then true else false" |> eval = true;;

let%test "test_trace1" = parse "if (if true then false else true) then true else false" |> trace = [If (If (True, False, True), True, False);
If (False, True, False);
False];;

let%test "test_trace2" = parse "if (if true then false else true) then true else false" |> eval = false;;

let%test "task6" = parse "true and false" |> eval = false;