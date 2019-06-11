[@@@ocamlhs_suite]

((1 + 2) * (4 + 5)) + (11 / 2) - 1

[@@@ocamlhs_suite_test]
"(getProgramResult res) `shouldBe` (RInt 31)"
[@@@end ocamlhs_suite]