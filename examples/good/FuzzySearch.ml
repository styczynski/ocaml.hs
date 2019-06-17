let rec min a b = if a <= b then a else b
let rec max a b = if a >= b then a else b

let rec levenshtein s t =
   let rec dist i j = match (i,j) with
      | (i,0) -> i
      | (0,j) -> j
      | (i,j) ->
         if nth s (i-1) == nth t (j-1) then dist (i-1) (j-1)
         else let d1, d2, d3 = dist (i-1) j, dist i (j-1), dist (i-1) (j-1) in
         1 + min d1 (min d2 d3)
   in
   dist (length s) (length t)

let rec test s t =
  printf " %s -> %s = %d\n" s t (levenshtein s t)

;;

test ["k";"i";"t";"t";"e";"n"] ["s";"i";"t";"t";"i";"n";"g"]
