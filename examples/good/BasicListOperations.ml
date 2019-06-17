(* Reversing a list *)
let rev list =
    let rec aux acc = function
        | [] -> acc
        | h::t -> aux (cons h acc) t in
    aux [] list

;;

rev ["a"; "b"; "c"]