{
  open Parser
}

let white = [' ' '\t']+
let num = ['0'-'9']|['1'-'9']['0'-'9']*

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "-" { MINUS }
  | "+" { PLUS }
  | "*" { MULT }
  | "/" { DIV }
  | num { CONST (Lexing.lexeme lexbuf) }
  | eof { EOF }
