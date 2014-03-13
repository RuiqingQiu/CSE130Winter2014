type token =
  | Num of (int)
  | EOF
  | TRUE
  | FALSE
  | Id of (string)
  | LET
  | REC
  | EQ
  | IN
  | FUN
  | IF
  | ELSE
  | LBRACE
  | RBRACE
  | LPAREN
  | RPAREN
  | PLUS
  | MINUS
  | MUL
  | DIV
  | LT
  | LE
  | NE
  | AND
  | OR
  | LBRACK
  | RBRACK
  | SEMI
  | COLONCOLON

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Nano.expr
