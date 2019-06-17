(*
 * This file provides some higher order functions as well
 * as some tasty applicative-like interface for veriants.
 *)

(* Map list with index *)
let rec mapi fn l =
    let rec helper fn l index = match l with
        | [] -> []
        | (h::t) -> cons (fn h index) (helper fn t (index+1))
    in helper fn l 0

(* Map list without index *)
let rec map fn l =
    let rec mapper item _ = fn item in mapi mapper l

(* Filter list items with index *)
let rec filteri fn l =
    let rec helper fn l index = match l with
        | [] -> []
        | (h::t) -> let rest = helper fn t (index+1) in
            if (fn h index) then cons h rest else rest
    in helper fn l 0

(* Filter list items without index *)
let rec filter fn l =
    let rec filterer item _ = fn item in filteri filterer l

(* Only iterate over list, ignore result *)
let rec iteri fn l = ignore (mapi fn l)
let rec iter fn l = ignore (map fn l)

(* Define computation abstraction *)
type ('a,'b) Computation = Bad of 'b | Ok of 'a

let rec fmap f = function
     | (Bad b) -> Bad b
     | (Ok a) -> Ok (f a)
let return v = Ok v
let pure v = Ok v

(* The same thing as with Haskell applicatives *)
let (<*>) compFn compVal = match (compFn, compVal) with
    | ((Bad b), _) -> Bad b
    | ((Ok fn), (Bad b)) -> Bad b
    | ((Ok fn), (Ok v)) -> Ok (fn v)

let (<$>) fn arg = fmap fn arg

(* Define lift: ('a -> 'b -> 'c) -> ('a,'d) Computation -> ('b,'e) Computation -> ('c,'f) Computation *)
let liftA2 f a b = (f <$> a) <*> b

;;

printf "Map list          :  %s\n" (  let fn arg = arg+1 in map fn [1;3;4]  ) ~
printf "Filter list       :  %s\n" (  let fn arg = arg > 2 in filter fn [1;3;4]  ) ~
printf "Return            :  %s\n" (  return 3  ) ~
printf "Applicative       :  %s\n" (  (return (fun arg -> arg+1)) <*> (return 3)  ) ~
printf "Some usage of <$> :  %s\n" (  ((+) <$> Ok 3) <*> Ok 5  ) ~
printf "Lift it!          :  %s\n" (  liftA2 (cons) (Ok 3) (Ok [4])  )