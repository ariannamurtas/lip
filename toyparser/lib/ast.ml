type ast =
    Const of int
  | Add of ast * ast
  | Sub of ast * ast
  | Mult of ast * ast
  | Div of ast * ast
  | Neg of ast

