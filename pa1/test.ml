(* CSE 130 PA 1. Autotester *)

#use "misc.ml"
#use "tester.ml" 

let sampleTests =
  [
  (fun () -> mkTest
    sumList
    [1;2;3;4]
    10
    "sample: sumList 1"
  );
  (fun () -> mkTest 
    sumList 
    [1;-2;3;5] 
    7 
    "sample: sumList 2"
  ); 
  (fun () -> mkTest 
    sumList 
    [1;3;5;7;9;11]
    36 
    "sample: sumList 3"
  ); 
  (fun () -> mkTest 
    digitsOfInt 
    3124 
    [3;1;2;4] 
    "sample: digitsOfInt 1"
  ); 
  (fun () -> mkTest 
    digitsOfInt 
    352663 
    [3;5;2;6;6;3] 
    "sample: digitsOfInt 2"
  ); 
  (fun () -> mkTest 
    digits
    31243
    [3;1;2;4;3] 
    "sample: digits 1"
  ); 
  (fun () -> mkTest 
    digits
    (-23422)
    [2;3;4;2;2]
    "sample: digits 2"
  ); 
  (fun () -> mkTest 
    additivePersistence 
    9876 
    2 
    "sample: additivePersistence1"
  ); 
  (fun () -> mkTest 
    digitalRoot 
    9876 
    3 
    "sample: digitalRoot"
  ); 
  (fun () -> mkTest 
    listReverse
    [1;2;3;4] 
    [4;3;2;1]
    "sample: reverse 1"
  ); 
  (fun () -> mkTest 
    listReverse 
    ["a";"b";"c";"d"]
    ["d";"c";"b";"a"] 
    "sample: rev 2"
  ); 
  (fun () -> mkTest 
    palindrome 
    "malayalam" 
    true
    "sample: palindrome 1"
  ); 
  (fun () -> mkTest 
    palindrome 
    "myxomatosis" 
    false
    "sample: palindrome 2"
  )] 

let yourTests =
  []

let doTest f = 
  try f () with ex -> 
    Printf.sprintf "WARNING: INVALID TEST THROWS EXCEPTION!!: %s \n\n"
    (Printexc.to_string ex)

let _ =
  let report = List.map doTest (sampleTests @ yourTests) in
  let _ = List.iter print130 (report@([scoreMsg()])) in
  let _ = print130 ("Compiled\n") in
  (!score, !max)

