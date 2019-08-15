(**   Test: Testing basic arithmetics type control                 **)
(**   Describe: should throw                                       **)
(**   Throws:  .*Cannot match types, expected: Bool, got: Int.*    **)

(* This will fail because of x + bool is invalid *)
let fn x = x+true in fn