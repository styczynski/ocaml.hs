let x = ala + 2;;

let z =
    let x = 3 in
    let y = 2 * x in
    x*y;;

let k x y = x + y;;

for i = 10 downto 1 do
  let s = string_of_int i in
  print_endline s
done;;
