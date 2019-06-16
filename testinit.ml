type Maybe = Nothing | Just of int
type Either = Left of int | Right of int

let (==) = value_eq in
let (<) = value_lt in
let (>) = value_gt in
let (<=) = value_lt_eq in
let (>=) = value_gt_eq in
let cons = value_cons in
let (||) = value_or in
let (&&) = value_and in
let (!) = value_not in
let (-) = value_sub in
let (+) = value_add in
let (*) = value_mul in
let (/) = value_div in
let ref = value_create_ref in
let set = value_set_ref in

let rec length_aux len = function
    [] -> len
  | _::l -> length_aux (len + 1) l
in

let tl = function
    [] -> failwith "tl"
  | _::l -> l
in

let hd = function
    [] -> failwith "hd"
  | a::_ -> a
in

let nth l n =
  if n < 0 then invalid_arg "List.nth" else
  let rec nth_aux l n =
    match l with
    | [] -> failwith "nth"
    | a::l -> if n == 0 then a else nth_aux l (n-1)
  in nth_aux l n
in

let nth_opt l n =
  if n < 0 then invalid_arg "List.nth" else
  let rec nth_aux l n =
    match l with
    | [] -> None
    | a::l -> if n == 0 then Some a else nth_aux l (n-1)
  in nth_aux l n
in

let rec append l1 l2 =
  match l1 with
    [] -> l2
  | a :: l -> cons a (append l l2)
in

let (@) = append in

export_env 0
;;
