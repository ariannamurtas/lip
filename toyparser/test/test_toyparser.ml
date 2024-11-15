open Toyparser.Main

let%test "test_eval_1" = parse "1 + 2 + 3 + (1 + 2)" |> eval = Ok 9

(* YOUR TESTS HERE *)

let%test "test_eval_2" = parse "1 - -1" |> eval = Ok 2

let%test "test_eval_3" = parse "0X3F + 1" |> eval = Ok 64