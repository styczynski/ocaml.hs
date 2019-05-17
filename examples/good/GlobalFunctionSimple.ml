[@@@ocamlhs_suite]

let foo x y = (x * y)
let bar x y = (x + y)
(foo 2 3) + (bar 1 2)

[@@@ocamlhs_suite_test]
"(getProgramResult res) `shouldBe` (RInt 9)"
[@@@end ocamlhs_suite]