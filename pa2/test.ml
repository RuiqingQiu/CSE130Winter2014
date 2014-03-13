(* CSE 130 PA 2. Autotester *)

#use "tester.ml" 
#use "misc.ml"
#use "nano.ml"

let collatz n =
  if n = 1 then 1
  else if n mod 2 = 0 then n/2
  else 3*n + 1

let testStrExpr e =
  let s = strExpr 0 e in
  (String.length s, s)

let e1 = BinaryOp (Var "x", Plus, Int 1)
let e2 = Fun ("x", e1)
let e3 = App (Var "succ", Int 5)
let e4 = Let ("succ", e2, e3)

let e5 = Let ("five", Int 5, App (Var "succ", Var "five"))
let e6 = Let ("succ", e2, e5)

let e7 = If (BinaryOp (Var "y", Lt, Var "z"), Var "z", Var "y")
let e8 = Fun ("y", Fun ("z", e7))
let e9 = Let ("max", e8, App (App (Var "max", Int 5), Int 3))

let sampleTests =
  [
  (fun () -> mkTest
     assoc
     (-1, "william", [("ranjit",85);("william",23);("moose",44)])
     23
     "sample: assoc 1"
  );
  (fun () -> mkTest 
    assoc
    (-1, "bob", [("ranjit",85);("william",23);("moose",44)])
    (-1)
    "sample: assoc 2"
  ); 
  (fun () -> mkTest 
    removeDuplicates
    [1;6;2;4;12;2;13;6;9]
    [1;6;2;4;12;13;9]
    "sample: removeDuplicates 2"
  );
  (fun () -> mkTest 
    removeDuplicates
    [1;1;1]
    [1]
    "sample: removeDuplicates 2"
  );

  (fun () -> mkTest 
    wwhile 
    ((fun x -> let xx = x*x*x in (xx, xx < 100)), 2) 
    512 
    "sample: wwhile 1"
  ); 
  (fun () -> mkTest 
    fixpoint
    ((fun x -> truncate (1e6 *. cos (1e-6 *. float x))), 0)
    739085
    "sample: fixpoint 1"
  ); 
  (fun () -> mkTest fixpoint (collatz,1) 1 "sample: fixpoint 2");
  (fun () -> mkTest fixpoint (collatz,3) 1 "sample: fixpoint 2");
  (fun () -> mkTest fixpoint (collatz,48) 1 "sample: fixpoint 2");
  (fun () -> mkTest fixpoint (collatz,107) 1 "sample: fixpoint 2");
  (fun () -> mkTest fixpoint (collatz,9001) 1 "sample: fixpoint 2");

  (fun () -> mkTest indent 0 "" "indent 0");
  (fun () -> mkTest indent 1 "  " "indent 1");
  (fun () -> mkTest indent 2 "    " "indent 2");

  (fun () -> mkTest
    testStrExpr e1
    (5, "x + 1")
    "strExpr e1"
  );
  (fun () -> mkTest
    testStrExpr e2
    (24, "function (x) {
  x + 1
}")
    "strExpr e2"
  );
  (fun () -> mkTest
    testStrExpr e3
    (8, "succ (5)")
    "strExpr e3"
  );
  (fun () -> mkTest
    testStrExpr e4
    (55, "let succ =
  function (x) {
    x + 1
  }
in
  succ (5)")
    "strExpr e4"
  );
  (fun () -> mkTest
    testStrExpr e6
    (84, "let succ =
  function (x) {
    x + 1
  }
in
  let five =
    5
  in
    succ (five)")
    "strExpr e6"
  );
  (fun () -> mkTest
    testStrExpr e7
    (31, "if (y < z) {
  z
} else {
  y
}")
    "strExpr e7"
  );
  (fun () -> mkTest
    testStrExpr e9
    (134, "let max =
  function (y) {
    function (z) {
      if (y < z) {
        z
      } else {
        y
      }
    }
  }
in
  max (5) (3)")
    "strExpr e9"
  );
 ] 

let e10 = App (Var "bool", BinaryOp (Int 1, And, Int 1))
let yourTests = 
 [
  (fun () -> mkTest
    testStrExpr e10
    (13, "bool (1 && 1)")
    "strExpr e10"
  );
 ] 

let doTest f = 
  try f () with ex -> 
    Printf.sprintf "WARNING: INVALID TEST THROWS EXCEPTION!!: %s \n\n"
    (Printexc.to_string ex)

let _ =
  let report = List.map doTest (sampleTests @ yourTests) in
  let _ = List.iter print130 (report@([scoreMsg()])) in
  let _ = print130 ("Compiled\n") in
  (!score, !max)

