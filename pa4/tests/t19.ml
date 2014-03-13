let realHd=hd in let hd=fun hd->(realHd hd)::hd in hd (hd
((1::(2::(3::(4::[]))))::[]))
