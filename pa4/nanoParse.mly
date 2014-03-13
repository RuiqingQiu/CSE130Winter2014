%{
(* See this for a tutorial on ocamlyacc 
 * http://plus.kaist.ac.kr/~shoh/ocaml/ocamllex-ocamlyacc/ocamlyacc-tutorial/ *)
open Nano 

%}

%token <int> Num
%token EOF
%token TRUE FALSE
%token <string> Id
%token LET
%token REC
%token EQ
%token IN
%token FUN
%token IF
%token ELSE
%token LBRACE
%token RBRACE
%token LPAREN
%token RPAREN

%token PLUS
%token MINUS
%token MUL
%token DIV
%token LT
%token LE
%token NE
%token AND
%token OR

%token LBRACK
%token RBRACK
%token SEMI
%token COLONCOLON

%start prog
%type <Nano.expr> prog

%%

prog :
| exp EOF { $1 }
exp :
| LPAREN exp RPAREN		{$2}
| LPAREN exp exp RPAREN		{ App ($2, $3) }
| LET Id EQ exp IN exp 		{Let ($2, $4, $6) }
| LET REC Id EQ exp IN exp	{LetRec($3, $5, $7) }
| FUN LPAREN Id RPAREN LBRACE exp RBRACE {Fun ($3, $6)} 
| IF LPAREN exp RPAREN LBRACE exp RBRACE ELSE LBRACE exp RBRACE 
				{If ($3, $6, $10)}
| LPAREN exp PLUS exp RPAREN	{ BinaryOp ($2, Plus, $4)}
| LPAREN exp MINUS exp RPAREN	{ BinaryOp ($2, Minus, $4)}
| LPAREN exp MUL exp RPAREN	{ BinaryOp ($2, Mul, $4)}
| LPAREN exp DIV exp RPAREN	{ BinaryOp ($2, Div, $4)}
| LPAREN exp LT exp RPAREN	{ BinaryOp ($2, Lt, $4)}
| LPAREN exp LE exp RPAREN	{ BinaryOp ($2, Le, $4)}
| LPAREN exp NE exp RPAREN	{ BinaryOp ($2, Ne, $4)}
| LPAREN exp AND exp RPAREN	{ BinaryOp ($2, And, $4)}
| LPAREN exp OR exp RPAREN	{ BinaryOp ($2, Or, $4) }
| exp EQ exp			{ BinaryOp ($1, Eq, $3) }
| exp COLONCOLON exp 		{ BinaryOp ($1, Cons, $3)}
| LBRACK RBRACK			{ Nil}
| LBRACK exp 			{ $2 }
| exp SEMI exp			{ BinaryOp ($1, Cons, $3)}
| exp RBRACK			{ BinaryOp ($1, Cons, Nil)}
| Num                           { Int $1 }
| TRUE				{ Bool(true) }
| FALSE 			{ Bool(false) }
| Id				{ Var ($1) }

