(* binop : ('a -> 'a -> 'a) -> 'a list -> 'a list *)
let binop op = function
  | b::a::r -> (op a b)::r
  | _ -> failwith "invalid expression"
 
(* interp : float list -> string -> string * float list *)
let interp s = function
  | "+" -> "add",    binop ( +. ) s
  | "-" -> "subtr",  binop ( -. ) s
  | "*" -> "mult",   binop ( *. ) s
  | "/" -> "divide", binop ( /. ) s
  | "^" -> "exp",    binop ( ** ) s
  | str -> "push", (float_of_string str) :: s
 
(* interp_and_show : float list -> string -> float list *)
let interp_and_show s inp =
  let op,s' = interp s inp in
  Printf.printf "%s\t%s\t" inp op;
  List.(iter (Printf.printf "%F ") (rev s'));
  print_newline ();
  s'
 
(* rpn_eval : string -> float list *)
let rpn_eval str =
  Printf.printf "Token\tAction\tStack\n";
  let ss = Str.(split (regexp_string " ") str) in
  List.fold_left interp_and_show [] ss



















type t =
    | Null
    | Empty
    | Lit of char
    | Union of t * t
    | Concat of t * t
    | Star of t

  let rec is_successful = function
    | Null -> false
    | Empty -> true
    | Lit _ -> false
    | Union (t, t') -> is_successful t || is_successful t'
    | Concat (t, t') -> is_successful t && is_successful t'
    | Star t -> true

  let rec compile_once = function
    | Null -> Null
    | Empty -> Empty
    | Lit c -> Lit c
    | Union (Null, t) -> t
    | Union (t, Null) -> t
    | Union (t, t') when t = t' -> t
    | Union (t, t') -> Union (compile_once t, compile_once t')
    | Concat (Null, t) -> Null
    | Concat (t, Null) -> Null
    | Concat (Empty, t) -> t
    | Concat (t, Empty) -> t
    | Concat (t, t') -> Concat (compile_once t, compile_once t')
    | Star Null -> Null
    | Star Empty -> Empty
    | Star t -> Star (compile_once t)

  let rec until_stable f x = if f x = x then x else until_stable f (f x)

  let compile = until_stable compile_once

  let rec step t c =
    match t with
    | Null -> Null
    | Empty -> Null
    | Lit c' when c' = c -> Empty
    | Lit c' -> Null
    | Union (t', t'') -> Union (step t' c, step t'' c)
    | Concat (t', t'') ->
        if is_successful t' then
          Union (Concat (step t' c, t''), step t'' c)
        else
          Concat (step t' c, t'')
    | Star t' -> Concat (step t' c, Star t')

  let step_and_compile t c = compile (step t c)

  let run t s = String.fold s ~init:t ~f:step_and_compile

  let accepts t s = is_successful (run t s)

  let string s = String.fold s ~init:Empty ~f:(fun t c -> Concat (t, Lit c))

  let any_of s =
    let cs = String.to_list s in
    compile (List.fold cs ~init:Null ~f:(fun t c -> Union (t, Lit c)))
