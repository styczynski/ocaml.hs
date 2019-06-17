
type 'a List = End | Element of 'a * ('a List)

let rec customLast p = match p with
    | End -> None
    | (Element (el, End)) -> Some el
    | (Element (_, tail)) -> customLast tail

;;

customLast (Element (0, Element (5, Element (9, Element (2, End)))))