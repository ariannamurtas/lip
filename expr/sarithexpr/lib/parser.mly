%{
open Ast
%}

%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token IF
%token THEN
%token ELSE
%token EOF
%token AND
%token OR
%token NOT
%token SUCC
%token PRED
%token ISZERO
%token ZERO

%left OR
%left AND
%right NOT
%right ISZERO
%right SUCC PRED

%start <expr> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | e0 = expr; AND; e1 = expr; { And(e0,e1) }
  | e0 = expr; OR; e1 = expr; { Or(e0,e1) }
  | NOT; e0 = expr; { Not(e0) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr; { If(e1, e2, e3) }
  | SUCC; e1 = expr; { Succ(e1) }
  | PRED; e1 = expr; { Pred(e1) }
  | ISZERO; e1 = expr; { IsZero(e1) }
  | ZERO { Zero }
  | LPAREN; e=expr; RPAREN {e}
;

