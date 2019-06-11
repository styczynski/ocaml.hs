[@@@ocamlhs_suite]

let myfunction x y = (
    let a = x * 14 in
    let b = x * 2 + a in
    let c = a + b in
    a + b + c + y
)

myfunction 1 2

[@@@ocamlhs_suite_test]
"(getProgramResult res) `shouldBe` (RInt 62)"
[@@@end ocamlhs_suite]