---
title: Getting Started
---

# Getting Started

Firstly clone all the source code from the repository:
```
    $ git clone http://github.com/styczynski/ocaml.hs.git
    $ cd ocaml.hs
```

Then you should install the Stack. It's a building tool for Haskell:
```
    $ curl -sSL https://get.haskellstack.org/ | sh
```

And build the executable:
```
    $ make
```

To build the web version (you are browsing it now) please execute the following command:
```
    $ make web
```

Now you are ready to play areound with the interpreter:
```
    $ stack exec interpreter -- --help
```
This command will print some help info.

To run some of the examples please type:
```
    $ stack exec interpreter -- -f ./examples/good/Trees.ml
```