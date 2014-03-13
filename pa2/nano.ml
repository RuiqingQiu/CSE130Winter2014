(* CSE 130: Programming Assignment 2
 * nano.ml
 * Author: Ruiqing Qiu
 * Login: rqiu
 * Date: 1/22/2014
 *)
type binary_operator = 
  | Plus
  | Minus
  | Mul
  | Div
  | Eq
  | Ne
  | Lt
  | Le
  | And
  | Or

type expr =   
  | Int of int
  | Bool of bool
  | Var of string
  | BinaryOp of expr * binary_operator * expr
  | Let of string * expr * expr
  | If of expr * expr * expr
  | Fun of string * expr
  | App of expr * expr

(* Function Name: indent
 * Argument: val indent : int -> string = <fun>
 * Description: indent will take in an int and indent that number of 
                tabs and return a string containing same amount of spaces
 *)
let indent k = 
  if k >= 0 then String.make (k*2) ' ' 
            else failwith "Number is negative"

let strBinOp : binary_operator -> string =
function
  | Plus -> "+"
  | Minus -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Eq -> "="
  | Ne -> "<>"
  | Lt -> "<"
  | Le -> "<="
  | And -> "&&"
  | Or -> "||"

(* Function name: strExpr
 * Argument: int -> expr -> string = <fun>
 * Description: strExpr will take in an int that represent its depth, which
 *              will be used as the amount of tab to indent and a type
 *              expr to evaluates to a nice formated string that will
 *              be printed.
 *)
let rec strExpr depth e =
  match e with
    | Int i              -> string_of_int i 
    | Bool b             -> string_of_bool b 
    | Var s              -> s 
    | BinaryOp (e1, b_op, e2) 
	                 -> strExpr depth e1 ^ " " ^ strBinOp b_op ^ " " ^ 
	                    strExpr depth e2
    | Let (s, e1, e2)    -> "let " ^ s ^ " =\n"
                            ^ indent (depth+1) ^ strExpr (depth+1) e1 ^ "\n"
			    ^ indent (depth) ^ "in\n"
			    ^ indent (depth+1) ^ strExpr (depth+1) e2
    | If (e1, e2, e3)    -> "if (" ^ strExpr depth e1 ^ ") {\n"
                            ^ indent (depth+1) ^ strExpr depth e2 ^ "\n"
			    ^ indent depth ^ "} else {\n"
			    ^ indent (depth+1) ^ strExpr depth e3 ^ "\n"
			    ^ indent (depth) ^ "}"
    | Fun (s, e1)        -> "function (" ^ s ^") {\n"
                            ^ indent (depth+1) 
	                    ^ strExpr (depth+1) e1 ^ "\n"
                            ^ indent (depth) ^ "}"
    | App (e1, e2)       -> strExpr depth e1 ^ " (" ^ strExpr depth e2 ^")"

(* Function name: printExpr : expr -> int = <fun> 
 * Description: will print out expression that are type expr into nice
 *              formating with indentation 
 *)
let printExpr e =
  let s = strExpr 0 e in
  let _ = Printf.printf "%s\n" s in
  String.length s
  

(* Expression for testing 
let e1 = BinaryOp (Var "x", Plus, Int 1);;
let e2 = Fun ("x", e1);;
let e3 = App (Var "succ", Int 5);;

let e4 = Let ("succ",e2,e3);;
let e5 = Let ("five", Int 5, App (Var "succ", Var "five"));;
let e6 = Let ("succ",e2,e5);;
let e7 = If (BinaryOp (Var "y", Lt, Var "z"), Var "z", Var "y");;
let e8 = Fun ("y", Fun ("z", e7));;
let e9 = Let ("max", e8, App (App (Var "max", Int 5), Int 3));;
*)
