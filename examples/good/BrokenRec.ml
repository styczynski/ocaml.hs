let rec fn = function
    | [] -> 0
    | (h::t) -> 1 + fn t
in fn [3;4;5]