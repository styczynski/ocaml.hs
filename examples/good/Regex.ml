(*
 * This file provides a regular expressions simple matching library.
 * You can test if a given string input matches the regular expression.
 *
 * Due to simplicity regular expressions are specified using RPN notation:
 *
 *     <token>  - Push string literal onto stack
 *     *        - Take a Kleene star of top element
 *     |        - Take union of two top elements
 *     +        - Concatenate two top elements
 *     null     - Null token
 *     eps      - Epsilon string
 *     ..       - Space
 *
 *  By default tokens are spearated by spaces and after the entire RPN parsing all
 *  left tokens on a stack are glued together using concatenation.
 *  So expression:
 *     "abc def"
 *  is equivalent to
 *     "abc def +"
 *
 *  So if you want to type eps as literal and to epsilon string just use + operator:
 *     "ep s +"
 *)

(* Regular expression type *)
type Regex =
    | Null
    | Empty
    | Lit of String
    | Union of Regex * Regex
    | Concat of Regex * Regex
    | Star of Regex

(* Pretty printer for regular expressions *)
let rec regex_to_str r = match r with
    | Null -> ""
    | Empty -> "eps"
    | (Lit str) -> "'" ^ str ^ "'"
    | (Union (l,r)) -> "((" ^ (regex_to_str l) ^ "|" ^ (regex_to_str r) ^ "))"
    | (Concat (l,r)) -> "(" ^ (regex_to_str l) ^ ")(" ^ (regex_to_str r) ^ ")"
    | (Star e) -> "(" ^ (regex_to_str e) ^ ")*"

(* Check if run was accepting one *)
let rec is_successful arg = match arg with
    | Null -> false
    | Empty -> true
    | (Lit _) -> false
    | (Union (t1, t2)) -> (is_successful t1) || (is_successful t2)
    | (Concat (t1, t2)) -> (is_successful t1) && (is_successful t2)
    | (Star t) -> true

(* Simplifies regular expression *)
let rec compile_once arg = match arg with
    | Null -> Null
    | Empty -> Empty
    | (Lit c) -> Lit c
    | (Union (Null, t)) -> t
    | (Union (t, Null)) -> t
    | (Union (t, u)) -> if t == u then t else Union (compile_once t, compile_once u)
    | (Concat (Null, t)) -> Null
    | (Concat (t, Null)) -> Null
    | (Concat (Empty, t)) -> t
    | (Concat (t, Empty)) -> t
    | (Concat (t, u)) -> Concat (compile_once t, compile_once u)
    | (Star Null) -> Null
    | (Star Empty) -> Empty
    | (Star t) -> Star (compile_once t)

(* This find a fixpoint *)
let rec until_stable fn arg = if (fn arg) == arg then arg else until_stable fn (fn arg)

(* Compile input regular expression *)
let rec compile = until_stable compile_once

(* Perform single step of automata *)
let rec step t c =
    match t with
    | Null -> Null
    | Empty -> Null
    | (Lit c2) -> if c == c2 then Empty else Null
    | (Union (u, w)) -> Union (step u c, step w c)
    | (Concat (u, w)) ->
        if is_successful u then
          Union (Concat (step u c, w), step w c)
        else
          Concat (step u c, w)
    | (Star u) -> Concat (step u c, Star u)

(* Compile once and proceed *)
let rec step_and_compile t c = compile (step t c)

(* Run automata on a given input *)
let rec run t s = foldl step_and_compile t s

(* Check if automata run is accepting *)
let rec accepts t s = is_successful (run t s)

(* Helper to evaluate RPN binary actions *)
let binop op = function
  | b::a::r -> cons (op a b) r
  | _ -> failwith "invalid expression"

(* Helper to evaluate RPN unary actions *)
let uniop op = function
  | a::r -> cons (op a) r
  | _ -> failwith "invalid expression"

(* Interpret RPN tokens *)
let interp s =
  let concatOp a b = Concat (a,b) in
  let unionOp a b = Union (a,b) in
  let starOp a = Star a in
  let litOp a = Lit a in
  let createLit a = match a with
    | "null" -> Null
    | "eps" -> Empty
    | ".." -> Lit " "
    | str -> foldl (concatOp) Empty (map litOp (drop 1 (string_split "" str)))
  in function
    | "*" -> "star",      uniop (starOp) s
    | "+" -> "concat",    binop (concatOp) s
    | "|" -> "union",     binop (unionOp) s
    | str -> "push", (cons (createLit str) s)

(* Helper function to run interp function *)
let interp_rpn s inp =
  let (op,s2) = interp s inp in s2

(* Parse string to Regex object *)
let rpn_eval str =
  let ss = string_split " " str in
  let results = foldl interp_rpn [] ss in
  let final_fold_fn acc e = match acc with
    | Null -> e
    | sth -> Concat (e,sth)
  in
    foldl final_fold_fn Null results

let test_cnt = ref 0
let regex_matches reg str = let r = rpn_eval reg in
    let result = accepts r (drop 1 (string_split "" str)) in
        (test_cnt := (!test_cnt) + 1) ~
        printf "Regex test %d | %30s   |  %20s     | %10s\n" (!test_cnt) ("("^reg^")") ("'"^str^"'") result ~
        result

;;

printf "\n %60s \n" "====== RPN REGEX TESTING =====" ~

(* This is equivalent to /a( z*|q* )/ *)
regex_matches "a z * q * |" "azzzz" ~

(* This is equivalent to /(ala|bartek) ma kota/ *)
regex_matches "ala bartek | .. ma .. kota" "ala ma kota" ~
regex_matches "ala bartek | .. ma .. kota" "bartek ma kota" ~
regex_matches "ala bartek | .. ma .. kota" "ala makota" ~

(* This is equivalent to /(a|b)( a* )z/ *)
regex_matches "a b | a * + z" "baaaz" ~
regex_matches "a b | a * + z" "baaabz" ~

printf "\n" ~
ignore ""