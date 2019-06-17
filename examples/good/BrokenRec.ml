let counter = ref 0 in
while (!counter) < 3 do
    print (!counter) ~
    counter := !counter+1
done