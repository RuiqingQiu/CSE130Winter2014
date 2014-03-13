let rec a = function (m) { function (n) { if ((m = 0)) { (n+1) } else { if
  ((n = 0)) { ((a (m-1)) 1) } else { ((a (m-1)) ((a m) (n-1))) } } }} in ((a 3) 4)
