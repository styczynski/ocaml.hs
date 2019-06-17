(*
 * Implementation of Levenstein distance along with few other utilities like
 * custom sorting function.
 *
 *)

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

let rec levenshtein_generate_score i l = match l with
        | [] -> []
        | (h::t) ->
            let score = levenshtein i h in
            let subscore = levenshtein_generate_score i t in
            cons (h,score) subscore

let rec insert cmp e l = match l with
    | [] -> [e]
    | (h :: t) -> if cmp e h <= 0 then cons e (cons h t) else cons h (insert cmp e t)
    | xxx -> print xxx ~ failwith "EEE1"

let rec sort cmp l = match l with
    | [] -> []
    | (h :: t) -> insert cmp h (sort cmp t)
    | xxx -> print xxx ~ failwith "EEE2"

let rec levenshtein_score i l =
    let scores = levenshtein_generate_score i l in
    let scores_sorted = sort (fun (_, v1) (_, v2) -> if v1 < v2 then 0-1 else if v1 > v2 then 1 else 0) scores in
    let get_first (s,_) = s in
    map get_first (take 5 scores_sorted)

;;

levenshtein_score ["k";"i";"t";"t";"e";"n"] [["s";"i";"t";"t";"i";"n";"g"];["x";"y";"z"]]
