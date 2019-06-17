let is_valid c = c >= 1

let get b (posx, posy) = array_get b (posx + posy * 9)

let get_as_string b pos =
  let i = get b pos in
  if is_valid i then string_of_int i else "."

let with_val b (posx, posy) v =
  let b = array_copy b in
  array_set b (posx + posy * 9) v ~ b

let of_list l =
  let b = array_make 81 0 in
  let rec loopx ypos el xpos = array_set b (xpos + ypos * 9) (if (el >= 0) && (el <= 9) then el else el) in
  let rec loopy rel ypos = iteri (loopx ypos) rel in
  (iteri loopy l) ~ b

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

;;

let sudoku_in = [[0; 0; 4;  8; 0; 0;  0; 1; 7];
              [6; 7; 0;  9; 0; 0;  0; 0; 0];
              [5; 0; 8;  0; 3; 0;  0; 0; 4];
              [3; 0; 0;  7; 4; 0;  1; 0; 0];
              [0; 6; 9;  0; 0; 0;  7; 8; 0];
              [0; 0; 1;  0; 6; 9;  0; 0; 5];
              [1; 0; 0;  0; 8; 0;  3; 0; 6];
              [0; 0; 0;  0; 0; 6;  0; 9; 1];
              [2; 4; 0;  0; 0; 1;  5; 0; 0]] in

let sudoku = of_list sudoku_in in
sudoku_print sudoku