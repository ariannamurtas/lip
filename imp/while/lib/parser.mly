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

%token EQ
%token LEQ
%token <string>CONST
%token <string>VAR
%token ADD
%token SUB
%token MUL
%token SKIP
%token ASSIGN
%token SEQ
%token WHILE
%token DO

%left OR
%left AND
%right NOT


%start <cmd> prog

%%

prog:
  | e = cmd; EOF { e }
;


expr:
  | TRUE { True }
  | FALSE { False }
  | e0 = expr; AND; e1 = expr; { And(e0,e1) }
  | e0 = expr; OR; e1 = expr; { Or(e0,e1) }
  | NOT; e0 = expr; { Not(e0) }
  | LPAREN; e=expr; RPAREN {e}

  | e0 = CONST { Const( int_of_string e0)}
  | e0 = VAR { Var(e0) }
  | e0 = expr; ADD; e1 = expr; { Add(e0,e1) }
  | e0 = expr; SUB; e1 = expr; { Sub(e0,e1) }
  | e0 = expr; MUL; e1 = expr; { Mul(e0,e1) }
  | e0 = expr; EQ; e1 = expr; { Eq(e0,e1) }
  | e0 = expr; LEQ; e1 = expr; { Leq(e0,e1) }
;

cmd:
  | SKIP { Skip }
  | e0 = VAR; ASSIGN; e1 = expr; { Assign(e0,e1) }
  | e0 = cmd; SEQ; e1 = cmd; { Seq(e0,e1) }
  | IF; e1 = expr; THEN; e2 = cmd; ELSE; e3 = cmd; { If(e1, e2, e3) }
  | WHILE; e0 = expr; DO; e1 = cmd; { While(e0,e1) }
;