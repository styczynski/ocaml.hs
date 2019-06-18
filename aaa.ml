let fn1 = fun f -> fun x -> f x
let fn2 = fun f x -> f x
let fn3 = let h f x = f x in h

let gg1 x = x+1
let gg2 = fun x -> x+1

;;

printf "=> %d\n" (fn1 gg1 0) ~
printf "=> %d\n" (fn2 gg1 0) ~
printf "=> %d\n" (fn3 gg1 0) ~
printf "=> %d\n" (fn1 gg2 0) ~
printf "=> %d\n" (fn2 gg2 0) ~
printf "=> %d\n" (fn3 gg2 0) ~
printf "=> %d\n" (fn1 ((fun k -> fun x -> x+k) 1) 0) ~
printf "=> %d\n" (fn1 ((fun k x -> x+k) 1) 0) ~
printf "=> %d\n" (fn2 (fun x -> x+1) 0) ~
printf "=> %d\n" (fn3 (fun x -> x+1) 0) ~

printf "# %s\n" (fn3 (fun x -> x+1))
