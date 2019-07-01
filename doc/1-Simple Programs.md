---
title: Simple Programs
---

# Simple Programs

Let's use the web version to code some programs.

Firstly let's try to define global variable:
```
    let k = 2
```

Now let's define a function that sums two numbers and apply it partially:
```
    let fn x y = x+y in
    fn k
```

You can try it here:

<iframe style="border:none; width:100%" src="http://styczynski.in/ocaml.hs/parser_index.html?let%20k%20%3D%202%20in%0Alet%20fn%20x%20y%20%3D%20x+y%20in%0Afn%20k"></iframe>

Let's play around with pattern matching and custom types:

```
(*
 * Example of custom list implementation done with help of variant types.
 *)

type 'a List = End | Element of 'a * ('a List)

(* Find the last element in the list *)
let rec customLast p = match p with
    | End -> None
    | (Element (el, End)) -> Some el
    | (Element (_, tail)) -> customLast tail

;;

(* Example usage *)
customLast (Element (0, Element (5, Element (9, Element (2, End)))))
```

<iframe style="border:none; width:100%" src="http://styczynski.in/ocaml.hs/parser_index.html?%28*%0A%20*%20Example%20of%20custom%20list%20implementation%20done%20with%20help%20of%20variant%20types.%0A%20*%29%0A%0Atype%20%27a%20List%20%3D%20End%20%7C%20Element%20of%20%27a%20*%20%28%27a%20List%29%0A%0A%28*%20Find%20the%20last%20element%20in%20the%20list%20*%29%0Alet%20rec%20customLast%20p%20%3D%20match%20p%20with%0A%20%20%20%20%7C%20End%20-%3E%20None%0A%20%20%20%20%7C%20%28Element%20%28el%2C%20End%29%29%20-%3E%20Some%20el%0A%20%20%20%20%7C%20%28Element%20%28_%2C%20tail%29%29%20-%3E%20customLast%20tail%0A%0A%3B%3B%0A%0A%28*%20Example%20usage%20*%29%0AcustomLast%20%28Element%20%280%2C%20Element%20%285%2C%20Element%20%289%2C%20Element%20%282%2C%20End%29%29%29%29%29"></iframe>
