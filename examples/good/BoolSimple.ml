[@@@ocamlhs_suite]

(true || false) && true

[@@@ocamlhs_suite_test]
"(getProgramResult res) `shouldBe` (RBool True)"
[@@@end ocamlhs_suite]