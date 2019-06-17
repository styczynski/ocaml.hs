type 'a Maybe = None | Some of 'a
type ('a, 'b) Either = Left of 'a | Right of 'b

let ( == ) = value_eq
let ( < ) = value_lt
let ( > ) = value_gt
let ( <= ) = value_lt_eq
let ( >= ) = value_gt_eq
let cons = value_cons
let ( || ) = value_or
let ( && ) = value_and
let ( ! ) = value_get_ref
let ( - ) = value_sub
let ( + ) = value_add
let ( * ) = value_mul
let ( / ) = value_div
let ref = value_create_ref
let ( := ) = value_set_ref

let rec length_aux len = function
    [] -> len
  | _::l -> length_aux (len + 1) l

let tl = function
    [] -> failwith "tl"
  | _::l -> l

let hd = function
    [] -> failwith "hd"
  | a::_ -> a

let nth l n =
  if n < 0 then invalid_arg "List.nth" else
  let rec nth_aux l n =
    match l with
    | [] -> failwith "nth"
    | a::l -> if n == 0 then a else nth_aux l (n-1)
  in nth_aux l n

let nth_opt l n =
  if n < 0 then invalid_arg "List.nth" else
  let rec nth_aux l n =
    match l with
    | [] -> None
    | a::l -> if n == 0 then Some a else nth_aux l (n-1)
  in nth_aux l n

let rec repeat v = function
    | 0 -> []
    | x -> if x > 0 then cons v (repeat v (x-1)) else []

let rec append l1 l2 =
  match l1 with
    [] -> l2
  | a :: l -> cons a (append l l2)

let length = length_aux 0

let ( @@ ) = append

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

let rec take n l =
    let rec filterer _ ind = ind < n in filteri filterer l

let rec drop n l =
    let rec filterer _ ind = ind >= n in filteri filterer l

let rec set_nth l index v =
    (take index l) @@ [v] @@ (drop (index+1) l)

type 'a array = array of ['a] Ref

let rec array_to_list = function
    | (array l) -> !l

let array_from_list l = array (ref l)

let array_get l index = match l with
    | (array l) -> nth (!l) index

let array_copy = fun l -> array_from_list (array_to_list l)

let array_make count v = array_from_list (repeat v count)

let array_set l index v = match l with
    | (array l) -> l := (set_nth (!l) index v)

let rec array_print = fun l -> print (array_to_list l)