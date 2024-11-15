%{
open Ast
%}

%token <string> CONST
%token PLUS
%token MINUS
%token LPAREN
%token RPAREN
%token EOF
%token MULT
%token DIV

%left MINUS
%left PLUS
%left MULT
%left DIV


%start <ast> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | n = CONST { Const(int_of_string n) }
  | e1 = expr; PLUS; e2 = expr { Add(e1,e2) }
  | e1 = expr; MINUS; e2 = expr { Sub(e1,e2) }
  | e1 = expr; MULT; e2 = expr { Mult(e1,e2) }
  | e1 = expr; DIV; e2 = expr { Div(e1,e2) }
  | MINUS; e1 = expr { Neg(e1) }
  | LPAREN; e=expr; RPAREN {e}
;
