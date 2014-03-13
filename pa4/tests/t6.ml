let f = function (g) {
  let x = 0 in 
  (g 2)
} in

let x = 100 in

let h = function (y) { (x + y) } in

(f h)
