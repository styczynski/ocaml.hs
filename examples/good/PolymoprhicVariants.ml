let tree = `Int 3
let four = `Float 5
let five = `Int "five"
let uni = `Uni
let g = [tree; four; uni]

;;

match tree with
    | ((`Int x) | (`Float x)) -> x
    | _ -> 0