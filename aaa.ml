let rec fn = fun f v -> f v

;;
fn (fun rr -> rr+1) 0

(*let tt = (fn 0) (fun r -> r+1) 9 in tt + 0*)