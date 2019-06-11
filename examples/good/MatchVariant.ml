type bintree =
        | Leaf
        | Tree of int
;;
match (Tree 9) with Leaf -> 8 | (Tree x) -> x | _ -> 1 ;;
