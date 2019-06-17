(*
 * This file contains prelude code loaded when interpreter is launched.
 *)

(* Basic polymorphic types definitions *)
type 'a Maybe = None | Some of 'a
type ('a, 'b) Either = Left of 'a | Right of 'b

(* Define basic operators *)
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
let ( ^ ) = string_join
let not a = value_not a
let ( % ) = value_mod
let ref = value_create_ref
let ( := ) = value_set_ref

(* Min/max *)
let rec min a b = if a <= b then a else b
let rec max a b = if a >= b then a else b

(* Returns length of list plus the given initial len *)
let rec length_aux len = function
    [] -> len
  | _::l -> length_aux (len + 1) l

(* Tail of list *)
let tl = function
    [] -> failwith "tl"
  | _::l -> l

(* Head of list *)
let hd = function
    [] -> failwith "hd"
  | a::_ -> a

(* N-th element of a list starting from 0 inclusively *)
let nth l n =
  if n < 0 then invalid_arg "List.nth" else
  let rec nth_aux l n =
    match l with
    | [] -> failwith "nth"
    | a::l -> if n == 0 then a else nth_aux l (n-1)
  in nth_aux l n

(* N-th variant that returns Maybe *)
let nth_opt l n =
  if n < 0 then invalid_arg "List.nth" else
  let rec nth_aux l n =
    match l with
    | [] -> None
    | a::l -> if n == 0 then Some a else nth_aux l (n-1)
  in nth_aux l n

(* Repeat given value N-times creating a list *)
let rec repeat v = function
    | 0 -> []
    | x -> if x > 0 then cons v (repeat v (x-1)) else []

(* Append two lists *)
let rec append l1 l2 =
  match l1 with
    [] -> l2
  | a :: l -> cons a (append l l2)

(* Length of a list *)
let rec length = length_aux 0

(* Append operator *)
let ( @@ ) = append

(* Map function over list with indexes *)
let rec mapi fn l =
    let rec helper fn l index = match l with
        | [] -> []
        | (h::t) -> cons (fn h index) (helper fn t (index+1))
    in helper fn l 0

(* Map function over list *)
let rec map fn l =
    let rec mapper item _ = fn item in mapi mapper l

(* Filter list using predicate - variant with indexes *)
let rec filteri fn l =
    let rec helper fn l index = match l with
        | [] -> []
        | (h::t) -> let rest = helper fn t (index+1) in
            if (fn h index) then cons h rest else rest
    in helper fn l 0

(* Filter list using predicate - variant without indexes *)
let rec filter fn l =
    let rec filterer item _ = fn item in filteri filterer l

(* Left fold for list - variant with indexes *)
let rec foldli fn acc l =
    let rec helper fn acc l index = match l with
        | [] -> acc
        | (h::t) -> helper fn (fn acc h index) t (index+1)
    in helper fn acc l 0

(* Left fold for list - variant without indexes *)
let rec foldl fn acc l =
    let rec folder a e _ = fn a e in foldli folder acc l

(* Right fold for list - variant with indexes *)
let rec foldri fn acc l =
    let rec helper fn acc l index = match l with
        | [] -> acc
        | (h::t) -> (fn h (helper fn acc t (index+1)) index)
    in helper fn acc l 0

(* Right fold for list - variant without indexes *)
let rec foldr fn acc l =
    let rec folder e a _ = fn e a in foldri folder acc l

(* Iterate over list - works like map but discards results *)
let rec iteri fn l = ignore (mapi fn l)
let rec iter fn l = ignore (map fn l)

(* Reverse list *)
let rev list =
    let rec aux acc = function
        | [] -> acc
        | h::t -> aux (cons h acc) t in
    aux [] list

(* Take n elements from list start *)
let rec take n l =
    let rec filterer _ ind = ind < n in filteri filterer l

(* Remove n elements from list start *)
let rec drop n l =
    let rec filterer _ ind = ind >= n in filteri filterer l

(* Set n-th element of a list creating new one *)
let rec set_nth l index v =
    (take index l) @@ [v] @@ (drop (index+1) l)

(*
 * Definitions for mutable arrays
 * Arrays behave pretty much the same as lists but are mutable.
 *)

(* Wrapper type for arrays *)
type 'a array = array of ['a] Ref

(* Covert array to plain list *)
let rec array_to_list = function
    | (array l) -> !l

(* Create array from plain list *)
let array_from_list l = array (ref l)

(* Retrieve array element *)
let array_get l index = match l with
    | (array l) -> nth (!l) index

(* Create new copy of an array (shallow copy) *)
let array_copy l = array_from_list (array_to_list l)

(* Create new array filled with n duplications of the given value *)
let array_make count v = array_from_list (repeat v count)

(* Set array value *)
let array_set l index v = match l with
    | (array l) -> ignore (l := (set_nth (!l) index v))

(* Pretty printer for arrays *)
let rec array_print = fun l -> print (array_to_list l)

(*
 * Helper functions for strings manipulations
 *
 *)

(* Converts string to list of strings (splits to letters) *)
let rec to_letters str = drop 1 (string_split "" str)
(* Concatenates all strings in an array creating a new string *)
let rec from_letters l = foldr (^) "" l