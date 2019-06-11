[@@@ocamlhs_suite]

let x = 2 + 3
let y = 8 * 7
let z  = x + (x*y)

z+x

[@@@ocamlhs_suite_test]
"(getProgramResult res) `shouldBe` (RInt 290)"
[@@@end ocamlhs_suite]