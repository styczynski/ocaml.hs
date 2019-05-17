[@@@ocamlhs_suite]

let foo x = x+1

if (foo 0)>=1 then "OK" else "BAD"

[@@@ocamlhs_suite_test]
"(getProgramResult res) `shouldBe` (RString \"OK\")"
[@@@end ocamlhs_suite]