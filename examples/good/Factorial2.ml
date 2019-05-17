[@@@ocamlhs_suite]

let fac x = (if x<=1 then 1 else let y = x-1 in x*(fac y))
fac 6

[@@@ocamlhs_suite_test]
"(getProgramResult res) `shouldBe` (RInt 720)"
[@@@end ocamlhs_suite]