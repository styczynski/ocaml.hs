#!/bin/bash

cat ./examples/GlobalLet.ml | stack exec ocamlhs-exe -- -g
