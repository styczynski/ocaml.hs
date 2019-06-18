(*
 * This file contains example of very basic imperative programming.
 * For more please see Sudoku.ml
 *
 *)
let counter = ref 0 in
let counterMax = 3 in
while (!counter) < counterMax do
    printf "Counter = %d < %d\n" (!counter) counterMax ~
    counter := !counter+1
done