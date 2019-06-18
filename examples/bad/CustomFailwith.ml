(*
 * Note that failwith is just a IO monad exported using setNativeVariable
 * So you can create native Haskell IO monads that throw errors and they will be
 * captured and displayed along with tracing information.
 *)
let rec fn1 x = if x <= 0 then failwith "Custom error: x <= 0" else fn1 (x-1)
let fn2 x = fn1 (x+1)
let g = fun x -> fn2 (x-1)
let h = let i = let h = fun j -> g j in h in i

;;

h 5