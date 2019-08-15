(**   Test: Testing duplicate names in function params             **)
(**   Describe: should throw                                       **)
(**   Throws:  .*Duplicated variable names in patterns.*           **)

(* Do not accept duplicated function parameters (works also in deeply nested patterns) *)
let fn h h h = h in fn