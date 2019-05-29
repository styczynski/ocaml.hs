#!/bin/bash

unset -v latest
for dir in $(find ../.stack-work -type d -name "ocamlhs-web.jsexe" -print)
do
    ans=$(find ${dir} -type f -name "out.js" -print)
    [[ ${ans} -nt $latest ]] && latest=${dir}
done

mkdir -p ./public/parser > /dev/null 2> /dev/null

cp "$latest/rts.js" "./public/parser/parser.rts.js"
cp "$latest/lib.js" "./public/parser/parser.lib.js"
cp "$latest/out.js" "./public/parser/parser.out.js"
cp "$latest/runmain.js" "./public/parser/parser.runmain.js"
cp "$latest/all.js" "./public/parser/parser.all.js"

echo "$latest"