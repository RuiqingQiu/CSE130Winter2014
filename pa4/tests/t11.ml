let foldn = 
  function (f) { function (b) { function (n) {
    let rec loop = 
      function (i) { function (c) {
        if ((i <= n)) {
          ((loop (i+1)) ((f i) c))
        } else {
          c
        }
      }} in
    ((loop 0) b)
  }}} in

let sum = ((foldn (function (x) { function (y) { (x + y) }})) 0) in

(sum 10)
