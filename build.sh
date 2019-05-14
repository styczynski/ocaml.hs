#!/bin/bash

rm -r -f -d parser &&
stack exec bnfc -- -m -o parser syntax.cf &&
sed -i -e 's/module Main where/module TestSyntax where/g' ./parser/TestSyntax.hs &&
cd parser &&
stack exec happy -- -gca ParSyntax.y &&
stack exec alex -- -g LexSyntax.x &&
cd .. &&
stack build