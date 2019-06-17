(*
 * This is a Sudoku solver.
 * The code was ported from https://ocaml.org/learn/tutorials/99problems.html with minor changes
 * to the syntax, but the algorithm is the same.
 *
 * This program tries to brut force the input Sudoku
 * it extinsively uses mutable arrays, pattern matching, mutually recursive functions and stuff
 * and it's rather slow.
 *
 *)

(* Check if index is valid *)
let is_valid c = c >= 1

(* Get value on a board: 0 means nothing and x>0 means a number *)
let get b (posx, posy) = array_get b (posx + posy * 9)

(* Get board value on a given coordinate as a string *)
let get_as_string b pos =
  let i = get b pos in
  if is_valid i then string_of_int i else "."

(* Copies mutable board and sets the value in a given field *)
let with_val b (posx, posy) v =
  let b = array_copy b in
  array_set b (posx + posy * 9) v ~ b

(* Transform [[Int]] into mutable falttened array *)
let of_list l =
  let b = array_make 81 0 in
  let rec loopx ypos el xpos = array_set b (xpos + ypos * 9) (if (el >= 0) && (el <= 9) then el else el) in
  let rec loopy rel ypos = iteri (loopx ypos) rel in
  (iteri loopy l) ~ b

(* Pretty printing for the sudoku board *)
let sudoku_print b =
    for y = 0 to 8 do
        (for x = 0 to 8 do
          printf (if x == 0 then "%s" else if x % 3 == 0 then " | %s"
                  else "  %s")  (get_as_string b (x, y))
        done) ~
        (if y < 8 then
          if y % 3 == 2 then printf "\n--------+---------+--------\n"
          else printf "\n        |         |        \n"
        else printf "\n")
    done

(* Check available inputs for a given field coordinates *)
let available b (posx, posy) =
   let avail = array_make 10 true in
   (for i = 0 to 8 do
     ignore (
         array_set avail (get b (posx, i)) false ~
         array_set avail (get b (i, posy)) false
     )
   done) ~
   (let sq_x = posx - posx % 3 in let sq_y = posy - posy % 3 in
       (for posx = sq_x to sq_x + 2 do
           for posy = sq_y to sq_y + 2 do
             array_set avail (get b (posx, posy)) false
           done
       done) ~
       (let av = ref [] in
        (for i = 1 to 9 do
            if array_get avail i then av := cons i (!av) else ignore 0
        done) ~
        !av
       )
   )

(* Iterate over all fields - left to right, top to bottom *)
let next (posx, posy) = if posx < 8 then (posx+1, posy) else (0, posy+1)

(* Try to fill the undecided entries. *)
let rec fill b (posx, posy) =
   let rec try_values b (posx, posy) l = match l with
      | (v::l) ->
         (match fill (with_val b (posx, posy) v) (next (posx, posy)) with
          | (Some v) -> (Some v)
          | None -> try_values b (posx, posy) l)
      | [] -> None
   in
      if posy > 8 then (Some b) (* filled all entries *)
      else if is_valid(get b (posx, posy)) then fill b (next (posx, posy))
      else match available b (posx, posy) with
           | [] -> None (* no solution *)
           | l -> try_values b (posx, posy) l

(* Solver entrypoint *)
let sudoku_solve s = let filled = fill s (0,0) in
    match filled with
        | (Some b) -> b
        | None -> failwith "Sudoku has no solution :("

;;

(* Example sudoku input *)
let sudoku_in = [[0; 0; 4;  8; 0; 0;  0; 1; 7];
              [6; 7; 0;  9; 0; 0;  0; 0; 0];
              [5; 0; 8;  0; 3; 0;  0; 0; 4];
              [3; 0; 0;  7; 4; 0;  1; 0; 0];
              [0; 6; 9;  0; 0; 0;  7; 8; 0];
              [0; 0; 1;  0; 6; 9;  0; 0; 5];
              [1; 0; 0;  0; 8; 0;  3; 0; 6];
              [0; 0; 0;  0; 0; 6;  0; 9; 1];
              [2; 4; 0;  0; 0; 1;  5; 0; 0]] in

(* Solve the sudoku *)
let sudoku = of_list sudoku_in in
let solved = sudoku_solve sudoku in
printf "The given sudoku has the following solution:\n" ~
sudoku_print solved ~
"OK"
