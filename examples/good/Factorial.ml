[@@@ocamlhs_suite]

let fac x = (if x<=1 then 1 else x*(fac (x-1)))
fac 5

[@@@ocamlhs_suite_test]
"(getProgramResult res) `shouldBe` (RInt 120)"
[@@@end ocamlhs_suite]