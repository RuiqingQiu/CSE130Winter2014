let f = fun x-> fun l -> (if x=0 then hd else fun l-> hd (tl l)) l in let
l=1::(2::(3::(4::[]))) in (f 0 l)::((f 1 l)::((f 2 l)::[])) 
