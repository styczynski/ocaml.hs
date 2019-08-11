(**   Test: Testing variant type mismatch                          **)
(**   Describe: should throw                                       **)
(**   Throws:  .*Cannot match types, expected: Var, got: Var2.*    **)

type Var = A | B of Int
type Var2 = A2 | B2 of Int

;;

(* This should throw because variant types are mismatched *)
match (B 9) with
    | (B2 c) -> c