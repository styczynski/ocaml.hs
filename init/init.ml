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

let rec append l1 l2 =
  match l1 with
    [] -> l2
  | a :: l -> cons a (append l l2)

let length = length_aux 0

let ( @@ ) = append