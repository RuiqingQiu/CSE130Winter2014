(* CSE 130: Programming Assignment 2
 * misc.ml
 * Author: Ruiqing Qiu
 * Login: rqiu
 * Date: 1/21/2014
 *)

(* assoc : int * string * (string * int) list -> int
 * or more generally, assoc : 'a * 'b * ('b * 'a) list -> 'a
 * assoc (d,k,[(k1,v1);(k2,v2);(k3,v3);...]) searches the list for the 
 * first i such
 * that ki = k.  If such a ki is found, 
 * then vi is returned.  Otherwise, if no such
 *   ki exists in the list, d is returned.
 * e.g. (assoc (-1,"william",[("ranjit",85);("william",23);("moose",44)]))
 *        returns 23
 *      (assoc (-1,"bob",[("ranjit",85);("william",23);("moose",44)]))
 *        returns -1
 *
 *  ** your function should be tail recursive **
 *)

let rec assoc (d,k,l) = 
  match l with 
  | (k1,v1)::tl -> if k = k1 then v1 else assoc(d,k,tl)
  | [] -> d

(* removeDuplicates : int list -> int list 
 * or more generally, removeDuplicates : 'a list -> 'a list
 * (removeDuplicates l) is the list of elements of l with duplicates (second,
 * third ... occurrences) removed, and where the remaining elements 
 * appear in the same order as in l.
 * e.g. (removeDuplicates [1,6,2,4,12,2,13,6,9]) is [1,6,2,4,12,13,9]
 *
 *  ** your function "helper" should be tail recursive **
 * for this problem only, you may use the library function List.mem and List.rev
 *)

(* 
 * Function Name and argument: removeDuplicates: int list -> int list
 * Description: It removes all the duplicate numbers in the list. 
 *              It contains a tail recursive helper method that separate 
 *              the list into seen and rest everytime it will check if the 
 *              number is also seen, if so, it will not put it into our list 
 *		that needed to be returned. If not, then it 
 *              will be put in. Using List.mem built in function to check 
 *              if an element is already in the list seen. And use List.rev to
 *              reverse the list so that i will be the same ordering as the
 *              input list.
 *)
let removeDuplicates l = 
  let rec helper (seen,rest) = 
      match rest with 
      |  [] -> seen
      | h::t -> 
        let seen' = if List.mem h seen then seen else h::seen in
        let rest' = t in 
	  helper (seen',rest') 
  in
      List.rev (helper ([],l))


(* wwhile : (int -> int * bool) * int -> int
 * or more generally, ('a -> 'a * bool) * 'a -> 'a
 * wwhile (f,b) should call the function f 
 * on input b, to get a pair (b',c').
 * wwhile should continue calling f 
 * on b' to update the pair as long as c' is true
 *   once f returns a c' that is false, wwhile should return b'
 * e.g. let f x = let xx = x*x*x in (xx,xx<100);;
 *   wwhile (f,2) should return 512
 *
 *  ** your function should be tail recursive **
 *)
(* Function name: wwhile
 * Arguments: val wwhile: ('a -> 'a * bool) * 'a -> 'a = <fun>
 * Description: wwhile function takes a function and a initial value
 *              it will find the first number that doesn't satisify 
 *              criteria and return a false in the tuple returned by
 *              the function.
 *)
let rec wwhile (f,b) =  
  match f b with 
  | (x, y) -> if y then wwhile (f,x) else x 

(* fixpoint : (int -> int) * int -> int
 * or more generally, fixpoint : ('a -> 'a) * 'a -> 'a
 * fixpoint (f,b) repeatedly 
 * replaces b with f(b) until b=f(b) and then returns b
 * e.g. let g x = truncate (1e6 *. cos (1e-6 *. float x));;
 * fixpoint (g,0) should return 739085    
 * (this is because cos 0.739085 is approximately 0.739085)
 *)

(* Function name: fixpoint
 * Argument: val fixpoint: ('a -> 'a) * 'a -> 'a = <fun>
 * Description: fixpoint will find the first number that satisfy 
 *              these two conditions: when xi equal to f x_i-1,
 *              f xi = xi
 *              Uses wwhile function from above and internally a 
 *              function to determine if the conditions are met
 *)
let fixpoint (f,b) = wwhile (
   (fun b -> let b' = (f b) in
        if b' = b then (b',false)
        else (b',true))
    ,b)

(************** Add Testing Code Here ***************)
