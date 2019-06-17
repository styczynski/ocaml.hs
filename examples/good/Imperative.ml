let counter = ref 0 in
while (!counter) < 3 do
    printf "Counter = %d and %d\n" (!counter) 4 ~
    counter := !counter+1
done