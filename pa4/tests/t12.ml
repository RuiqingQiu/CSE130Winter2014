let foldn = 
  function (f) { function (b) { function (n) {
    let rec loop = 
      function (i) { function (c) {
        if ((i <= n)) { ((loop (i+1)) ((f i) c)) } else { c }
      }} in
    ((loop 0) b)
  }}} in

let fac = ((foldn (function (x) { function (y) { if ((x = 0)) { 1 } else { (x * y) } }} )) 1) in

(fac 10)
