let sum = function (x) { (x + 1) } in

let rec sum = function (x) {
  if ((x <= 0)) {
    0
  } else {
    (x + (sum (x-1)))
  }
} in

(sum 10)
