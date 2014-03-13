let rec fac = function (x) {
  if ((x <= 0)) { 1 } else { (x * (fac (x-1))) }
} in
(fac 10)
