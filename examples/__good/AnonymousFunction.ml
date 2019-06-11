[@@@ocamlhs_suite]

let foo x y = (let bar = (fun x -> (x+2)) in bar (x+y))
foo 10 5

[@@@ocamlhs_suite_test]
"(getProgramResult res) `shouldBe` (RInt 17)"
[@@@end ocamlhs_suite]