---
title: Home
---

# ocamlhs [![Coverage Status](https://coveralls.io/repos/github/styczynski/ocaml.hs/badge.svg?branch=master)](https://coveralls.io/github/styczynski/ocaml.hs?branch=master) [![Build Status](https://travis-ci.com/styczynski/ocaml.hs.svg?branch=master)](https://travis-ci.com/styczynski/ocaml.hs)


<iframe style="border:none; width:100%" src="http://styczynski.in/ocaml.hs/parser_index.html"></iframe>

You can try the [Fullscreen version here](http://styczynski.in/ocaml.hs/parser_index.html)

## Building

This project requires [Stack](https://docs.haskellstack.org/en/stable/README/) to build.
You can install it by doing:
```bash
   $ curl -sSL https://get.haskellstack.org/ | sh
```

Then please call the Makefile to build the executables:
```bash
  $ make
```

## Running interpreter

You can run the executable built by Stack using the following command:
```bash
   $ stack exec -- interpreter --help # Other paramters for interpreter
```

To run a file through the interpreter please call the following command:
```bash
   $ stack exec -- interpreter -f ./examples/good/Sudoku.ml
```

There are plenty of examples available including:
* Brute force Sudoku solver
* Regular expression RPN parsing
* Some higher order functions
* Manipulations on mutable data sources
* Fuzzy search with Levenstein distance + interactions with user

