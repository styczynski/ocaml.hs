[@@@ocamlhs_suite]

let foo x y = (x+y)

if (foo 1 2)>=(foo 2 3) then (foo 1 2) else (foo 2 3)

[@@@ocamlhs_suite_test]
"(getProgramResult res) `shouldBe` (RInt 5)"
[@@@end ocamlhs_suite]