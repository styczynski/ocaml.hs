
let rec mapi fn l =
    let rec helper fn l index = match l with
        | [] -> []
        | (h::t) -> cons (fn h index) (helper fn t (index+1))
    in helper fn l 0

let rec map fn l =
    let rec mapper item _ = fn item in mapi mapper l

let rec filteri fn l =
    let rec helper fn l index = match l with
        | [] -> []
        | (h::t) -> let rest = helper fn t (index+1) in
            if (fn h index) then cons h rest else rest
    in helper fn l 0

let rec filter fn l =
    let rec filterer item _ = fn item in filteri filterer l

let rec iteri fn l = ignore (mapi fn l)
let rec iter fn l = ignore (map fn l)

type ('a,'b) Computation = Bad of 'b | Ok of 'a

let rec fmap f = function
     | (Bad b) -> Bad b
     | (Ok a) -> Ok (f a)
let return v = Ok v
let (<*>) compFn compVal = match (compFn, compVal) with
    | ((Bad b), _) -> Bad b
    | ((Ok fn), (Bad b)) -> Bad b
    | ((Ok fn), (Ok v)) -> Ok (fn v)
;;

printf "%s\n" ((return (fun arg -> arg+1)) <*> (return 3)) ~
ignore(0)
