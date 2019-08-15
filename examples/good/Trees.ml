(*
 * This is example container for binary trees with labelled nodes.
 * It contains few very simple examples.
 *)

(*
 * Binary tree that has 'b values in leafs and 'a values in node
 *)
type ('a, 'b) Tree = Leaf of 'b | Node of (('a, 'b) Tree) * 'a * (('a, 'b) Tree)

(*
 * Fold left for 'a 'a trees
 * First goes into left subtree, then to node value and then to right subtree
 *)
let rec treeFold fn t acc = match t with
     | (Node (t1, n, t2)) -> treeFold fn t2 (fn (treeFold fn t1 acc) n)
     | (Leaf v) -> fn acc v

(* Retrieves a list of all values stored in 'a 'a tree *)
let treeVals t = let fn acc x = cons x acc in treeFold fn t []

(* Sum over all tree leafs *)
let treeSum t = treeFold (+) t 0
;;

(* Example tree *)
let tree = (Node ((Node ((Leaf 3), 2, (Node (Node ((Leaf 1),4,(Leaf 0)), 5, (Leaf 11))))), 1, (Node ((Leaf 0), 1, (Leaf 2))))) in
treeSum tree