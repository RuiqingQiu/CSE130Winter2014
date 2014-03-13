(* CSE 130: Programming Assignment 1
 * misc.ml
 * Author: Ruiqing Qiu
 * Login: rqiu
 * Date: 1/14/2014
 *)

(* sumList : int list -> int 
   ***** PUT DOCUMENTATION COMMENTS HERE *****
   A function sumList that take a type list and output the sum
   of each element in that list. if empty, it evaluates to 0
   (see the digits function below for an example of what is expected)
*) 

let rec sumList l =
  match l with
  | [] -> 0
  | (h::t) -> h + sumList t


(* digitsOfInt : int -> int list 
   ***** PUT DOCUMENTATION COMMENTS HERE *****
   A function digitsOfInt takes a type int and output each digit in
   a int list. If the number is less than 0, it evaluates to an empty 
   list.
   If not, then it will module 10 to get the least significant digit
   each time the function is called
 *)

let rec digitsOfInt n = 
  if (n < 0) then []  
  else
    if (n < 10) then [n] 
    else (digitsOfInt (n/10)) @ [(n mod 10)] 

(* digits : int -> int list
 * (digits n) is the list of digits of n in the order in which they appear
 * in n
 * e.g. (digits 31243) is [3,1,2,4,3]
 *      (digits (-23422) is [2,3,4,2,2]
 *)
 
let digits n = digitsOfInt (abs n)


(* From http://mathworld.wolfram.com/AdditivePersistence.html
 * Consider the process of taking a number, adding its digits, 
 * then adding the digits of the number derived from it, etc., 
 * until the remaining number has only one digit. 
 * The number of additions required to obtain a single digit from a number n 
 * is called the additive persistence of n, and the digit obtained is called 
 * the digital root of n.
 * For example, the sequence obtained from the starting number 9876 is (9876, 30, 3), so 
 * 9876 has an additive persistence of 2 and a digital root of 3.
 *)


(* ***** PROVIDE COMMENT BLOCKS FOR THE FOLLOWING FUNCTIONS ***** *)

(* additivePersistence: int -> int = <fun>
 * A function additivePersistence that takes in a type int and 
 * output the additive persistence of that number. Additive persistence
 * is obtained by adding each digits of that number, and do so until
 * the number is a single digit. Then return the number of times the sum
 * is done
 * Error Condition: When the argument is an negative number, 
 *                  The program will raise an exception
 *)
let rec additivePersistence n = 
  if n < 0 then failwith "Number is Negative"
  else 
    if n < 10 then 0
    else 1 + additivePersistence (sumList (digits n))

(* digitalRoot: int -> int = <fun>
 * A function digitalRoot that takes in a type int and 
 * output the digital root of that number. It will do the same thing
 * as the additive persistence is obtained by adding each digits of 
 * that number is a single digit. And return that value
 * Error Condition: When the argument is an negative number, 
 *                  The program will raise an exception
 *)
let rec digitalRoot n =
  if n < 0 then raise (Failure "Number is Negative")
  else 
    if n < 10 then n
    else digitalRoot (sumList (digits n))

(* listReverse: 'a list -> 'a list = <fun>
 * Function listReverse takes in 'a list and output 'a list with
 * the elements in that list reversed. 
 *)
let rec listReverse l = 
  match l with
  | [] -> []
  | h::t -> listReverse t @ [h]

(* explode : string -> char list 
 * (explode s) is the list of characters in the string s in the order in 
 *   which they appear
 * e.g.  (explode "Hello") is ['H';'e';'l';'l';'o']
 *)
let explode s = 
  let rec _exp i = 
    if i >= String.length s then [] else (s.[i])::(_exp (i+1)) in
  _exp 0

(* palindrome: string -> bool = <fun>
 * Function palidrome takes a string and output a boolean that 
 * indicates if the word is palindrome. It uses the explode method that
 * provided to seperate a string into a list of characters. Then it 
 * compare and output whether the reverse of that list is the same
 * as the original list, If so, then it's a palindrome and return true
 * else it return false
 *)
let palindrome w = 
  if explode w = listReverse (explode w) then true
  else false
