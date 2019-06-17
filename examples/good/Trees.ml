(*
 * Binary tree that has 'b values in leafs and 'a values in node
 *)
type ('a, 'b) Tree = Leaf of 'b | Node of (('a, 'b) Tree) * 'a * (('a, 'b) Tree)

(* Fold *)
let rec treeFold fn t acc = match t with
     | (Node (t1, n, t2)) -> treeFold fn t2 (fn (treeFold fn t1 acc) n)
     | (Leaf v) -> fn acc v

let treeVals t = let fn acc x = cons x acc in treeFold fn t []
let treeSum t = treeFold (+) t 0
;;

let tree = (Node ((Node ((Leaf 3), 2, (Node (Node ((Leaf 1),4,(Leaf 0)), 5, (Leaf 11))))), 1, (Node ((Leaf 0), 1, (Leaf 2))))) in
treeSum tree