[@@@ocamlhs_suite]

let x = 4

x * 2

[@@@ocamlhs_suite_test]
"(getProgramResult res) `shouldBe` (RInt 8)"
[@@@end ocamlhs_suite]