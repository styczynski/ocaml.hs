[@@@ocamlhs_suite]

let x = "ABC"
x

[@@@ocamlhs_suite_test]
"(getProgramResult res) `shouldBe` (RString \"ABC\")"
[@@@end ocamlhs_suite]