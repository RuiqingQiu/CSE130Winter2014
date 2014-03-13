
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
  | Cons
(* Expr added, Hd and Tl for extra credit for PA4 *)
type expr =   
  | Int of int
  | Bool of bool
  | Var of string
  | BinaryOp of expr * binary_operator * expr
  | Let of string * expr * expr
  | If of expr * expr * expr
  | Fun of string * expr
  | App of expr * expr
  | LetRec of string * expr * expr
  | Nil
  | Hd of expr
  | Tl of expr

exception NanoMLParseError of string
exception NanoMLEvalError of string
	
type value =  
  | VInt of int
  | VBool of bool
  | VClosure of env * string option * string * expr
  | VNil
  | VPair of value * value

and env = (string * value) list

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
  | Cons -> "::"

let spr = Printf.sprintf

let indent k = String.make (2 * k) ' '

let rec strValue : value -> string =
function
  | VInt i -> spr "%d" i
  | VBool b -> spr "%b" b
  | VNil -> "[]"
  | VPair (v1, v2) -> spr "(%s::%s)" (strValue v1) (strValue v2) 
  | VClosure (env, fo, x, e) -> 
      let fs = match fo with
        | None   -> "Anonymous"
        | Some f -> spr "Named %s" f in
      spr "{%s,%s,%s,%s}" (strEnv env) fs x (strExpr 0 e)

and strEnv : env -> string =
fun env ->
  let xs = List.map (fun (x,v) -> spr "%s:%s" x (strValue v)) env in
  spr "[%s]" (String.concat ";" xs)

and strExpr : int -> expr -> string =
fun depth e -> match e with
  | Int i ->
      string_of_int i
  | Bool b ->
      string_of_bool b
  | Var x ->
      x
  | BinaryOp (e1, op, e2) ->
      spr "(%s %s %s)" (strExpr depth e1) (strBinOp op) (strExpr depth e2)
  | If (e1, e2, e3) ->
      spr "%s%s%s%s%s"
        (spr "if %s {\n"    (strExpr depth e1))
        (spr "%s%s\n"       (indent (succ depth)) (strExpr (succ depth) e2))
        (spr "%s} else {\n" (indent depth))
        (spr "%s%s\n"       (indent (succ depth)) (strExpr (succ depth) e3))
        (spr "%s}"          (indent depth))
  | Let (x, e1, e2) ->
      spr "%s%s%s%s"
        (spr "let %s =\n" x)
        (spr "%s%s\n" (indent (succ depth)) (strExpr (succ depth) e1))
        (spr "%sin\n" (indent depth))
        (spr "%s%s"   (indent (succ depth)) (strExpr (succ depth) e2))
  | App (e1, e2) ->
      spr "(%s %s)" (strExpr depth e1) (strExpr depth e2)
  | Fun (x, e) ->
      spr "function (%s) {\n%s%s\n%s}" x
        (indent (succ depth)) (strExpr (succ depth) e)
      (indent depth)
  | Nil -> "[]"
  | LetRec (x, e1, e2) ->
      spr "%s%s%s%s"
        (spr "let rec %s =\n" x)
        (spr "%s%s\n" (indent (succ depth)) (strExpr (succ depth) e1))
        (spr "%sin\n" (indent depth))
        (spr "%s%s"   (indent (succ depth)) (strExpr (succ depth) e2))

(*********************** Misc **********************************************)

let binop op e1 e2 = BinaryOp (e1, op, e2)
let plus           = binop Plus
let mul            = binop Mul
let div            = binop Div
let minus          = binop Minus
let eq             = binop Eq
let cons           = binop Cons

(*********************** Some helpers you might need ***********************)

let rec fold f acc args = 
  match args with
    | []   -> acc
    | h::t -> fold f (f (acc,h)) t

let listAssoc (k,l) = 
  fold (function
    | None, (k',v) -> if k = k' then Some v else None
    | acc, _       -> acc
  ) None l

(*********************** Your code starts here ****************************)

(* Function name: lookup 
 * Evaluation: val lookup : string * env -> value
 * Description: look up will take a string of variable and look its value
 * in the enviornment. If find, then return type value else return
 * NanoMLEvalError
 *)
let lookup (x,env) = 
  match listAssoc (x, env) with
  | Some v -> v
  | _ -> raise (NanoMLEvalError ("variable not bound: " ^ x))

(* Function name: eval
 * Evaluation: val eval : env * expr -> value
 * Description: eval will take an tuple of env and expr, and evaluate
 * expr to get a type value
 *)
let rec eval (env,e) = 
  match e with 
  | Int i -> VInt i
  | Bool b -> VBool b
  | Var x -> lookup (x, env)
  | BinaryOp (e1, op, e2) -> 
     (let x = eval (env, e1) in
      let y = eval (env, e2) in
      match (x, op, y) with
      | VInt x, Plus, VInt y -> VInt (x + y)
      | VInt x, Minus, VInt y -> VInt (x - y)
      | VInt x, Mul, VInt y -> VInt (x * y)
      | VInt x, Div, VInt y -> VInt (x / y)
      | VInt x, Eq, VInt y -> VBool (x = y)
      | VBool x, Eq, VBool y -> VBool (x = y)
      | VInt x, Ne, VInt y -> VBool (x <> y)
      | VBool x, Ne, VBool y -> VBool (x <> y)
      | VInt x, Lt, VInt y -> VBool (x < y)
      | VInt x, Le, VInt y -> VBool (x <= y)
      | VBool x, And, VBool y -> VBool (x && y)
      | VBool x, Or, VBool y -> VBool (x || y)
      | x, Cons, y -> VPair (x, y)
      | _ -> raise (NanoMLEvalError("Error: operation can't be performed")))
  | If (e1, e2, e3) -> 
     (match eval (env, e1) with
      | VBool b -> if b then eval (env, e2) else eval (env, e3)
      | _ -> raise (NanoMLEvalError("Error: If can't performed")))
  | Let (str, e1, e2) -> 
     (let newEnv = (str, eval(env, e1))::env in eval (newEnv, e2))
  | LetRec (str, e1, e2) ->
     let x = 
        match eval (env, e1) with
        | VClosure(env, None, arg, e1) -> VClosure (env, Some str, arg, e1)
        | _ -> eval (env, e1) in
     (let rec newEnv = (str,x)::env in eval (newEnv, e2))
  | App (e1, e2) -> 
     (* this part is for extra credit, if the argument is hd, then call
      * eval with Hd, if tl then call with Tl
      *)
     (match e1 with
     | Var "hd" -> eval(env, Hd e2)
     | Var "tl" -> eval(env, Tl e2)
     | _ ->
     (let arg = eval (env, e2) in
        match eval (env, e1) with
	| VClosure(env', None, formalArg, a) ->
	  (* Bind formal argument with the actual argument into env*)
	  (* Get the function env together with the total env *)
	  eval((formalArg, arg) :: env' @ env, a)  
	| VClosure(env', Some n, formalArg, a) as f ->
	    (* Get the formal argument binded with arguments and
	     * name of the function with function defination *)
	    eval((formalArg, arg)::(n, f)::env' @ env, a)
	| _ -> raise (NanoMLEvalError("Not a function")))
    )
  | Fun (formalArg, e1) -> VClosure(env, None, formalArg, e1)
  (* extra credit part for Hd and Tl *)
  | Hd e1 ->
     (match eval (env, e1) with
      | VPair (x, y) -> x)
  | Tl e1 -> 
     (match eval (env, e1) with
      | VPair (x, y) -> y) 
  | Nil -> VNil

  | _ -> raise (NanoMLEvalError ("Not a valid operation"))
