let counter = ref 0 in
while (!counter) < 3 do
    print (!counter) ~ set counter (!counter+1)
done