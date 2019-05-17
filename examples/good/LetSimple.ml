[@@@ocamlhs_suite]

let foo x = (let d = 2 in d*x)

foo 3

[@@@ocamlhs_suite_test]
"(getProgramResult res) `shouldBe` (RInt 6)"
[@@@end ocamlhs_suite]