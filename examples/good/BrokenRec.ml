let cre z = ref z
let fn wuda =
    let r = cre (wuda + 7) in
    print r ~ print r ~ (r := 99) ~ print r ~ r

;;

fn 1

(*let counter = ref 0 in*)
(*while (!counter) < 3 do*)
(*    print (!counter) ~*)
(*    counter := !counter+1*)
(*done*)