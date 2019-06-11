let foo x y = (let bar = (fun x -> (x+2)) in bar (x+y))

foo 10 5