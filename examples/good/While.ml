let x = ref 0 in
while !x < 3 do
    print (!x) ~ set x (!x + 1)
done ;;