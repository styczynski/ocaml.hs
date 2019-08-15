(*
 * Example of custom list implementation done with help of variant types.
 *)

type 'a List = End | Element of 'a * ('a List)

(* Find the last element in the list *)
let rec customLast p = match p with
    | End -> None
    | (Element (el, End)) -> Some el
    | (Element (_, tail)) -> customLast tail

;;

(* Example usage *)
customLast (Element (0, Element (5, Element (9, Element (2, End)))))