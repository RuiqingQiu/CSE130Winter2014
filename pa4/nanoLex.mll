{
  open Nano        (* nano.ml *)
  open NanoParse   (* nanoParse.ml from nanoParse.mly *)
}

rule token = parse
    eof         { EOF }
  | [' ' '\r' '\t' '\n'] { token lexbuf } (* skip spaces *)
  | "let"	{ LET }
  | "rec" 	{ REC }
  | "="		{ EQ }
  | "in"	{ IN }
  | "function"  { FUN }
  | "if"	{ IF }
  | "else"	{ ELSE }
  | "{"		{ LBRACE }
  | "}"		{ RBRACE }
  | "("		{ LPAREN }
  | ")"		{ RPAREN }
  | "+"		{ PLUS }
  | "-"		{ MINUS }
  | "*"		{ MUL }
  | "/"		{ DIV }
  | "<"		{ LT }
  | "<="	{ LE }
  | "!="	{ NE }
  | "&&"	{ AND }
  | "||"	{ OR }
  | "[" 	{ LBRACK }
  | "]" 	{ RBRACK }
  | ";"		{ SEMI }
  | "::"	{ COLONCOLON }
  | "true"	{ TRUE }
  | "false"  	{ FALSE }
  | ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_']* as str
  		{ Id (str) }
  | ['0'-'9']+ as num
  		{ Num (int_of_string num) }
  | _           { raise (NanoMLParseError
                          ("Illegal Character '"^(Lexing.lexeme lexbuf)^"'")) }
