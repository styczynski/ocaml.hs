(* This should throw not in scope error because function has got no rec keyword*)
let fn x = fn (x+1) in fn