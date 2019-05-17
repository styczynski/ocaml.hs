[@@@ocamlhs_suite]

((1 > 2) && (2 < 3)) || (7 < 1)

[@@@ocamlhs_suite_test]
"(getProgramResult res) `shouldBe` (RBool True)"
[@@@end ocamlhs_suite]