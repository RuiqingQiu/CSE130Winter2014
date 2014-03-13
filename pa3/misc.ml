(* CSE 130: Programming Assignment 3
 * misc.ml
 * Author: Ruiqing Qiu
 * PID: A98022702
 * Date: 1/29/2014
 *)

(* For this assignment, you may use the following library functions:

   List.map
   List.fold_left
   List.fold_right
   List.split
   List.combine
   List.length
   List.append
   List.rev

   See http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html for
   documentation.
*)



(* Do not change the skeleton code! The point of this assignment is to figure
 * out how the functions can be written this way (using fold). You may only
 * replace the   failwith "to be implemented"   part. *)



(*****************************************************************)
(******************* 1. Warm Up   ********************************)
(*****************************************************************)

(* Function name: sqsum
 * Argument: val sqsum : int list -> int
 * Description: sqsum will compute the sum of each element's sqaure
 *              sqsum [1;2;3] = 1 + 4 + 9 = 14
 *              Uses fold_left with running result adding the current 
 *              element.
 *)
let sqsum xs = 
  let f a x = a + x*x in
  let base = 0 in
    List.fold_left f base xs

(* Function name: pipe
 * Argument: val pipe: ('a -> 'a) list -> ('a -> 'a)
 * Description: pipe function will take in a list of function as 
 *              [f1;f2;...;fn] and compute the value fn(..(f2(f1 x)))
 * This function use a helper function that will call the previous 
 * function in the list and nest these functions together
 *)
let pipe fs = 
  let f a x = fun y -> x (a y) in
  let base = fun x -> x in
    List.fold_left f base fs

(* Function name: sepConcat
 * Argument val sepConcat : string -> string list -> string
 * Description: this function will use the first argument as the seperator
 * to create a string that concat each element of the string list with
 * the seperator. Uses List.fold_left function.
 *)
let rec sepConcat sep sl = match sl with 
  | [] -> ""
  | h :: t -> 
      let f a x = a ^ sep ^ x in 
      let base = h in
      let l = t in
        List.fold_left f base l

(* Function name: stringOfList
 * Argument: val stringOfList : ('a -> string) -> 'a list -> string
 * Description: stringOfList will use first argument of a function to 
 * map each element and then create a string of elements that are in
 * list form that has a open and close sqaure bracket with semincolon to
 * separate each element
 *)
let stringOfList f l = 
  "[" ^ sepConcat "; " (List.map f l) ^ "]"

(*****************************************************************)
(******************* 2. Big Numbers ******************************)
(*****************************************************************)

(* Function name: clone
 * Argument: val clone : 'a -> int -> 'a list
 * Description: clone function will return a list of n copies (second arg)
 * of the value x. If n is less than or equal to 0, return empty list
 *)
let rec clone x n =  
  if n > 0 then let n = n-1 in x::clone x n
  else []

(* Function name: padZero
 * Argument: val padZero : int list -> int list -> int list * int list
 * Description: padZero takes two list and adds zero in front of the
 * short list to make the lists equal length.
 *)
let rec padZero l1 l2 = 
  let diff = List.length l1 - List.length l2 in
  (* List 1 is longer than List 2 *)
  if diff > 0
  then (l1, clone 0 diff @ l2)
  (* List 2 is longer than List 1 *)	
  else if diff < 0
  then (clone 0 (-diff) @ l1, l2)
  (* Both lists have the same length *)
  else (l1,l2)

(* Function name: removeZero
 * Argument: val removeZero : int list -> int list
 * Description: removeZero function takes a list and removes a prefix of
 * trailing zeros.
 *)
let rec removeZero l = 
  match l with
  | h::t -> if h = 0 then removeZero t
            else l
  | _ -> l

(* Function name: bigAdd
 * Argument: val bigAdd: int list -> int list -> int list
 * Description: bigAdd takes two integer lists, where each integer is 
 * between 0 and 9 and returns the list corresponding to the addition of
 * the two big-integers. Uses List.rev, List.combine and List.fold_left
 *)
let bigAdd l1 l2 = 
  let add (l1, l2) = 
    let f a x = 
      match x with 
      | (number1, number2) ->
        if number1 > 9 || number1 < 0 || number2 > 9 || number2 < 0 
	then failwith "the digit is invalid in the list"
	else match a with 
        | (carryout, res) -> ((number1 + number2 + carryout) / 10,
			(number1 + number2 + carryout) mod 10 :: res)  
    in
    let base = (0, []) in
    let args = List.combine (List.rev (0::l1)) (List.rev (0::l2)) in
    let (_, res) = List.fold_left f base args in
      res
  in 
    removeZero (add (padZero l1 l2))

(* Function name: mulByDigit
 * Argument: val mulByDigit : int -> int list -> int list
 * Description: mulByDigit takes an integer digit and a big integer,
 * and returns the big integer list which is the result of
 * multiplying the big integer with the digit. Using bigAdd as a helper
 * Function
 *)
let rec mulByDigit i l = 
  if i < 0 || i > 9 then failwith "the digit is invalid"
  else
  let rec helper numbers res = 
    if numbers > 0 then helper (numbers-1) (bigAdd l res ) else res 
  in helper i []

(* Function name: bigMul
 * Argument: val bigMul : int list -> int list -> int list
 * Description: bigMul takes two integer lists, where each integer is
 * between 0 and 9 and returns the list corresponding to the muliplication
 * of the two big-integers. Uses List.combine, List.length, List.rev and
 * List.fold_left functions.
 *)
let bigMul l1 l2 = 
  let f a x = 
    match x with
    | (numberList, digit) ->
      match a with
      | (zeroBehind, res) -> (zeroBehind + 1, 
          bigAdd res (mulByDigit digit (numberList @ (clone 0 zeroBehind))))
  in
  let base = (0, []) in
  let args = List.combine (clone l1 (List.length l2)) (List.rev l2) in
  let (_, res) = List.fold_left f base args in
    res
