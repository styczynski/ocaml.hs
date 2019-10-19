---
title: Examples
---

# Examples

This page provides you with preconfigured interpreter instances for basic and advanced demos delivered in /examples/ directory.

## Very basic imperative programming

[Fullscreen playground here](http://styczynski.in/ocaml.hs/parser_index.html?%28*%0A%20*%20This%20file%20contains%20example%20of%20very%20basic%20imperative%20programming.%0A%20*%20For%20more%20please%20see%20Sudoku.ml%0A%20*%0A%20*%29%0Alet%20counter%20%3D%20ref%200%20in%0Alet%20counterMax%20%3D%203%20in%0Awhile%20%28%21counter%29%20%3C%20counterMax%20do%0A%20%20%20%20printf%20%22Counter%20%3D%20%25d%20%3C%20%25d%5Cn%22%20%28%21counter%29%20counterMax%20%7E%0A%20%20%20%20counter%20%3A%3D%20%21counter+1%0Adone)

<iframe style="border:none; width:100%" src="http://styczynski.in/ocaml.hs/parser_index.html?%28*%0A%20*%20This%20file%20contains%20example%20of%20very%20basic%20imperative%20programming.%0A%20*%20For%20more%20please%20see%20Sudoku.ml%0A%20*%0A%20*%29%0Alet%20counter%20%3D%20ref%200%20in%0Alet%20counterMax%20%3D%203%20in%0Awhile%20%28%21counter%29%20%3C%20counterMax%20do%0A%20%20%20%20printf%20%22Counter%20%3D%20%25d%20%3C%20%25d%5Cn%22%20%28%21counter%29%20counterMax%20%7E%0A%20%20%20%20counter%20%3A%3D%20%21counter+1%0Adone"></iframe>

## Custom trees operations

[Fullscreen playground here](http://styczynski.in/ocaml.hs/parser_index.html?%28*%0A%20*%20This%20is%20example%20container%20for%20binary%20trees%20with%20labelled%20nodes.%0A%20*%20It%20contains%20few%20very%20simple%20examples.%0A%20*%29%0A%0A%28*%0A%20*%20Binary%20tree%20that%20has%20%27b%20values%20in%20leafs%20and%20%27a%20values%20in%20node%0A%20*%29%0Atype%20%28%27a%2C%20%27b%29%20Tree%20%3D%20Leaf%20of%20%27b%20%7C%20Node%20of%20%28%28%27a%2C%20%27b%29%20Tree%29%20*%20%27a%20*%20%28%28%27a%2C%20%27b%29%20Tree%29%0A%0A%28*%0A%20*%20Fold%20left%20for%20%27a%20%27a%20trees%0A%20*%20First%20goes%20into%20left%20subtree%2C%20then%20to%20node%20value%20and%20then%20to%20right%20subtree%0A%20*%29%0Alet%20rec%20treeFold%20fn%20t%20acc%20%3D%20match%20t%20with%0A%20%20%20%20%20%7C%20%28Node%20%28t1%2C%20n%2C%20t2%29%29%20-%3E%20treeFold%20fn%20t2%20%28fn%20%28treeFold%20fn%20t1%20acc%29%20n%29%0A%20%20%20%20%20%7C%20%28Leaf%20v%29%20-%3E%20fn%20acc%20v%0A%0A%28*%20Retrieves%20a%20list%20of%20all%20values%20stored%20in%20%27a%20%27a%20tree%20*%29%0Alet%20treeVals%20t%20%3D%20let%20fn%20acc%20x%20%3D%20cons%20x%20acc%20in%20treeFold%20fn%20t%20%5B%5D%0A%0A%28*%20Sum%20over%20all%20tree%20leafs%20*%29%0Alet%20treeSum%20t%20%3D%20treeFold%20%28+%29%20t%200%0A%3B%3B%0A%0A%28*%20Example%20tree%20*%29%0Alet%20tree%20%3D%20%28Node%20%28%28Node%20%28%28Leaf%203%29%2C%202%2C%20%28Node%20%28Node%20%28%28Leaf%201%29%2C4%2C%28Leaf%200%29%29%2C%205%2C%20%28Leaf%2011%29%29%29%29%29%2C%201%2C%20%28Node%20%28%28Leaf%200%29%2C%201%2C%20%28Leaf%202%29%29%29%29%29%20in%0AtreeSum%20tree)

<iframe style="border:none; width:100%" src="http://styczynski.in/ocaml.hs/parser_index.html?%28*%0A%20*%20This%20is%20example%20container%20for%20binary%20trees%20with%20labelled%20nodes.%0A%20*%20It%20contains%20few%20very%20simple%20examples.%0A%20*%29%0A%0A%28*%0A%20*%20Binary%20tree%20that%20has%20%27b%20values%20in%20leafs%20and%20%27a%20values%20in%20node%0A%20*%29%0Atype%20%28%27a%2C%20%27b%29%20Tree%20%3D%20Leaf%20of%20%27b%20%7C%20Node%20of%20%28%28%27a%2C%20%27b%29%20Tree%29%20*%20%27a%20*%20%28%28%27a%2C%20%27b%29%20Tree%29%0A%0A%28*%0A%20*%20Fold%20left%20for%20%27a%20%27a%20trees%0A%20*%20First%20goes%20into%20left%20subtree%2C%20then%20to%20node%20value%20and%20then%20to%20right%20subtree%0A%20*%29%0Alet%20rec%20treeFold%20fn%20t%20acc%20%3D%20match%20t%20with%0A%20%20%20%20%20%7C%20%28Node%20%28t1%2C%20n%2C%20t2%29%29%20-%3E%20treeFold%20fn%20t2%20%28fn%20%28treeFold%20fn%20t1%20acc%29%20n%29%0A%20%20%20%20%20%7C%20%28Leaf%20v%29%20-%3E%20fn%20acc%20v%0A%0A%28*%20Retrieves%20a%20list%20of%20all%20values%20stored%20in%20%27a%20%27a%20tree%20*%29%0Alet%20treeVals%20t%20%3D%20let%20fn%20acc%20x%20%3D%20cons%20x%20acc%20in%20treeFold%20fn%20t%20%5B%5D%0A%0A%28*%20Sum%20over%20all%20tree%20leafs%20*%29%0Alet%20treeSum%20t%20%3D%20treeFold%20%28+%29%20t%200%0A%3B%3B%0A%0A%28*%20Example%20tree%20*%29%0Alet%20tree%20%3D%20%28Node%20%28%28Node%20%28%28Leaf%203%29%2C%202%2C%20%28Node%20%28Node%20%28%28Leaf%201%29%2C4%2C%28Leaf%200%29%29%2C%205%2C%20%28Leaf%2011%29%29%29%29%29%2C%201%2C%20%28Node%20%28%28Leaf%200%29%2C%201%2C%20%28Leaf%202%29%29%29%29%29%20in%0AtreeSum%20tree"></iframe>

## Basic list operations

[Fullscreen playground here](http://styczynski.in/ocaml.hs/parser_index.html?%28*%0A%20*%20This%20file%20contains%20basic%20list%20operations.%0A%20*%29%0A%0A%28*%20Reversing%20a%20list%20*%29%0Alet%20rev%20list%20%3D%0A%20%20%20%20let%20rec%20aux%20acc%20%3D%20function%0A%20%20%20%20%20%20%20%20%7C%20%5B%5D%20-%3E%20acc%0A%20%20%20%20%20%20%20%20%7C%20h%3A%3At%20-%3E%20aux%20%28cons%20h%20acc%29%20t%20in%0A%20%20%20%20aux%20%5B%5D%20list%0A%3B%3B%0A%0Arev%20%5B%22a%22%3B%20%22b%22%3B%20%22c%22%5D)

<iframe style="border:none; width:100%" src="http://styczynski.in/ocaml.hs/parser_index.html?%28*%0A%20*%20This%20file%20contains%20basic%20list%20operations.%0A%20*%29%0A%0A%28*%20Reversing%20a%20list%20*%29%0Alet%20rev%20list%20%3D%0A%20%20%20%20let%20rec%20aux%20acc%20%3D%20function%0A%20%20%20%20%20%20%20%20%7C%20%5B%5D%20-%3E%20acc%0A%20%20%20%20%20%20%20%20%7C%20h%3A%3At%20-%3E%20aux%20%28cons%20h%20acc%29%20t%20in%0A%20%20%20%20aux%20%5B%5D%20list%0A%3B%3B%0A%0Arev%20%5B%22a%22%3B%20%22b%22%3B%20%22c%22%5D"></iframe>

## Higher order functions

[Fullscreen playground here](http://styczynski.in/ocaml.hs/parser_index.html?%28*%0A%20*%20This%20file%20provides%20some%20higher%20order%20functions%20as%20well%0A%20*%20as%20some%20tasty%20applicative-like%20interface%20for%20veriants.%0A%20*%29%0A%0A%28*%20Map%20list%20with%20index%20*%29%0Alet%20rec%20mapi%20fn%20l%20%3D%0A%20%20%20%20let%20rec%20helper%20fn%20l%20index%20%3D%20match%20l%20with%0A%20%20%20%20%20%20%20%20%7C%20%5B%5D%20-%3E%20%5B%5D%0A%20%20%20%20%20%20%20%20%7C%20%28h%3A%3At%29%20-%3E%20cons%20%28fn%20h%20index%29%20%28helper%20fn%20t%20%28index+1%29%29%0A%20%20%20%20in%20helper%20fn%20l%200%0A%0A%28*%20Map%20list%20without%20index%20*%29%0Alet%20rec%20map%20fn%20l%20%3D%0A%20%20%20%20let%20rec%20mapper%20item%20_%20%3D%20fn%20item%20in%20mapi%20mapper%20l%0A%0A%28*%20Filter%20list%20items%20with%20index%20*%29%0Alet%20rec%20filteri%20fn%20l%20%3D%0A%20%20%20%20let%20rec%20helper%20fn%20l%20index%20%3D%20match%20l%20with%0A%20%20%20%20%20%20%20%20%7C%20%5B%5D%20-%3E%20%5B%5D%0A%20%20%20%20%20%20%20%20%7C%20%28h%3A%3At%29%20-%3E%20let%20rest%20%3D%20helper%20fn%20t%20%28index+1%29%20in%0A%20%20%20%20%20%20%20%20%20%20%20%20if%20%28fn%20h%20index%29%20then%20cons%20h%20rest%20else%20rest%0A%20%20%20%20in%20helper%20fn%20l%200%0A%0A%28*%20Filter%20list%20items%20without%20index%20*%29%0Alet%20rec%20filter%20fn%20l%20%3D%0A%20%20%20%20let%20rec%20filterer%20item%20_%20%3D%20fn%20item%20in%20filteri%20filterer%20l%0A%0A%28*%20Only%20iterate%20over%20list%2C%20ignore%20result%20*%29%0Alet%20rec%20iteri%20fn%20l%20%3D%20ignore%20%28mapi%20fn%20l%29%0Alet%20rec%20iter%20fn%20l%20%3D%20ignore%20%28map%20fn%20l%29%0A%0A%28*%20Define%20computation%20abstraction%20*%29%0Atype%20%28%27a%2C%27b%29%20Computation%20%3D%20Bad%20of%20%27b%20%7C%20Ok%20of%20%27a%0A%0Alet%20rec%20fmap%20f%20%3D%20function%0A%20%20%20%20%20%7C%20%28Bad%20b%29%20-%3E%20Bad%20b%0A%20%20%20%20%20%7C%20%28Ok%20a%29%20-%3E%20Ok%20%28f%20a%29%0Alet%20return%20v%20%3D%20Ok%20v%0Alet%20pure%20v%20%3D%20Ok%20v%0A%0A%28*%20The%20same%20thing%20as%20with%20Haskell%20applicatives%20*%29%0Alet%20%28%3C*%3E%29%20compFn%20compVal%20%3D%20match%20%28compFn%2C%20compVal%29%20with%0A%20%20%20%20%7C%20%28%28Bad%20b%29%2C%20_%29%20-%3E%20Bad%20b%0A%20%20%20%20%7C%20%28%28Ok%20fn%29%2C%20%28Bad%20b%29%29%20-%3E%20Bad%20b%0A%20%20%20%20%7C%20%28%28Ok%20fn%29%2C%20%28Ok%20v%29%29%20-%3E%20Ok%20%28fn%20v%29%0A%0Alet%20%28%3C%24%3E%29%20fn%20arg%20%3D%20fmap%20fn%20arg%0A%0A%28*%20Define%20lift%3A%20%28%27a%20-%3E%20%27b%20-%3E%20%27c%29%20-%3E%20%28%27a%2C%27d%29%20Computation%20-%3E%20%28%27b%2C%27e%29%20Computation%20-%3E%20%28%27c%2C%27f%29%20Computation%20*%29%0Alet%20liftA2%20f%20a%20b%20%3D%20%28f%20%3C%24%3E%20a%29%20%3C*%3E%20b%0A%0A%3B%3B%0A%0A%28%28+%29%20%3C%24%3E%20Ok%203%29%20%3C*%3E%20Ok%205)

<iframe style="border:none; width:100%" src="http://styczynski.in/ocaml.hs/parser_index.html?%28*%0A%20*%20This%20file%20provides%20some%20higher%20order%20functions%20as%20well%0A%20*%20as%20some%20tasty%20applicative-like%20interface%20for%20veriants.%0A%20*%29%0A%0A%28*%20Map%20list%20with%20index%20*%29%0Alet%20rec%20mapi%20fn%20l%20%3D%0A%20%20%20%20let%20rec%20helper%20fn%20l%20index%20%3D%20match%20l%20with%0A%20%20%20%20%20%20%20%20%7C%20%5B%5D%20-%3E%20%5B%5D%0A%20%20%20%20%20%20%20%20%7C%20%28h%3A%3At%29%20-%3E%20cons%20%28fn%20h%20index%29%20%28helper%20fn%20t%20%28index+1%29%29%0A%20%20%20%20in%20helper%20fn%20l%200%0A%0A%28*%20Map%20list%20without%20index%20*%29%0Alet%20rec%20map%20fn%20l%20%3D%0A%20%20%20%20let%20rec%20mapper%20item%20_%20%3D%20fn%20item%20in%20mapi%20mapper%20l%0A%0A%28*%20Filter%20list%20items%20with%20index%20*%29%0Alet%20rec%20filteri%20fn%20l%20%3D%0A%20%20%20%20let%20rec%20helper%20fn%20l%20index%20%3D%20match%20l%20with%0A%20%20%20%20%20%20%20%20%7C%20%5B%5D%20-%3E%20%5B%5D%0A%20%20%20%20%20%20%20%20%7C%20%28h%3A%3At%29%20-%3E%20let%20rest%20%3D%20helper%20fn%20t%20%28index+1%29%20in%0A%20%20%20%20%20%20%20%20%20%20%20%20if%20%28fn%20h%20index%29%20then%20cons%20h%20rest%20else%20rest%0A%20%20%20%20in%20helper%20fn%20l%200%0A%0A%28*%20Filter%20list%20items%20without%20index%20*%29%0Alet%20rec%20filter%20fn%20l%20%3D%0A%20%20%20%20let%20rec%20filterer%20item%20_%20%3D%20fn%20item%20in%20filteri%20filterer%20l%0A%0A%28*%20Only%20iterate%20over%20list%2C%20ignore%20result%20*%29%0Alet%20rec%20iteri%20fn%20l%20%3D%20ignore%20%28mapi%20fn%20l%29%0Alet%20rec%20iter%20fn%20l%20%3D%20ignore%20%28map%20fn%20l%29%0A%0A%28*%20Define%20computation%20abstraction%20*%29%0Atype%20%28%27a%2C%27b%29%20Computation%20%3D%20Bad%20of%20%27b%20%7C%20Ok%20of%20%27a%0A%0Alet%20rec%20fmap%20f%20%3D%20function%0A%20%20%20%20%20%7C%20%28Bad%20b%29%20-%3E%20Bad%20b%0A%20%20%20%20%20%7C%20%28Ok%20a%29%20-%3E%20Ok%20%28f%20a%29%0Alet%20return%20v%20%3D%20Ok%20v%0Alet%20pure%20v%20%3D%20Ok%20v%0A%0A%28*%20The%20same%20thing%20as%20with%20Haskell%20applicatives%20*%29%0Alet%20%28%3C*%3E%29%20compFn%20compVal%20%3D%20match%20%28compFn%2C%20compVal%29%20with%0A%20%20%20%20%7C%20%28%28Bad%20b%29%2C%20_%29%20-%3E%20Bad%20b%0A%20%20%20%20%7C%20%28%28Ok%20fn%29%2C%20%28Bad%20b%29%29%20-%3E%20Bad%20b%0A%20%20%20%20%7C%20%28%28Ok%20fn%29%2C%20%28Ok%20v%29%29%20-%3E%20Ok%20%28fn%20v%29%0A%0Alet%20%28%3C%24%3E%29%20fn%20arg%20%3D%20fmap%20fn%20arg%0A%0A%28*%20Define%20lift%3A%20%28%27a%20-%3E%20%27b%20-%3E%20%27c%29%20-%3E%20%28%27a%2C%27d%29%20Computation%20-%3E%20%28%27b%2C%27e%29%20Computation%20-%3E%20%28%27c%2C%27f%29%20Computation%20*%29%0Alet%20liftA2%20f%20a%20b%20%3D%20%28f%20%3C%24%3E%20a%29%20%3C*%3E%20b%0A%0A%3B%3B%0A%0A%28%28+%29%20%3C%24%3E%20Ok%203%29%20%3C*%3E%20Ok%205"></iframe>

## Polymprphic variant types

[Fullscreen playground here](http://styczynski.in/ocaml.hs/parser_index.html?%28*%0A%20*%20Usage%20of%20Ocaml-like%20polymorphic%20variant%20types.%0A%20*%29%0A%0Alet%20basic_color_to_int%20%3D%20function%0A%20%20%7C%20%28%60Black%29%20-%3E%200%20%7C%20%28%60Red%29%20%20%20%20%20-%3E%201%20%7C%20%28%60Green%29%20-%3E%202%20%7C%20%28%60Yellow%29%20-%3E%203%0A%20%20%7C%20%28%60Blue%29%20%20-%3E%204%20%7C%20%28%60Magenta%29%20-%3E%205%20%7C%20%28%60Cyan%29%20%20-%3E%206%20%7C%20%28%60White%29%20%20-%3E%207%0A%0Alet%20color_to_int%20%3D%20function%0A%20%20%7C%20%28%60Basic%20%28basic_color%2C%20weight%29%29%20-%3E%0A%20%20%20%20let%20base%20%3D%20%28match%20weight%20with%20%28%60Bold%29%20-%3E%208%20%7C%20%28%60Regular%29%20-%3E%200%29%20in%0A%20%20%20%20base%20+%20basic_color_to_int%20basic_color%0A%20%20%7C%20%28%60RGB%20%28r%2Cg%2Cb%29%29%20-%3E%2016%20+%20b%20+%20g%20*%206%20+%20r%20*%2036%0A%0Alet%20extended_color_to_int%20%3D%20function%0A%20%20%7C%20%28%60RGBA%20%28r%2Cg%2Cb%2Ca%29%29%20-%3E%20256%20+%20a%20+%20b%20*%206%20+%20g%20*%2036%20+%20r%20*%20216%0A%20%20%7C%20%28%28%60Basic%20_%29%20%7C%20%28%60RGB%20_%29%29%20as%20col%20-%3E%20color_to_int%20col%0A%0A%3B%3B%0A%0Acolor_to_int%20%28%28%60Basic%20%28%28%60Black%29%2C%28%60Bold%29%29%29%29)

<iframe style="border:none; width:100%" src="http://styczynski.in/ocaml.hs/parser_index.html?%28*%0A%20*%20Usage%20of%20Ocaml-like%20polymorphic%20variant%20types.%0A%20*%29%0A%0Alet%20basic_color_to_int%20%3D%20function%0A%20%20%7C%20%28%60Black%29%20-%3E%200%20%7C%20%28%60Red%29%20%20%20%20%20-%3E%201%20%7C%20%28%60Green%29%20-%3E%202%20%7C%20%28%60Yellow%29%20-%3E%203%0A%20%20%7C%20%28%60Blue%29%20%20-%3E%204%20%7C%20%28%60Magenta%29%20-%3E%205%20%7C%20%28%60Cyan%29%20%20-%3E%206%20%7C%20%28%60White%29%20%20-%3E%207%0A%0Alet%20color_to_int%20%3D%20function%0A%20%20%7C%20%28%60Basic%20%28basic_color%2C%20weight%29%29%20-%3E%0A%20%20%20%20let%20base%20%3D%20%28match%20weight%20with%20%28%60Bold%29%20-%3E%208%20%7C%20%28%60Regular%29%20-%3E%200%29%20in%0A%20%20%20%20base%20+%20basic_color_to_int%20basic_color%0A%20%20%7C%20%28%60RGB%20%28r%2Cg%2Cb%29%29%20-%3E%2016%20+%20b%20+%20g%20*%206%20+%20r%20*%2036%0A%0Alet%20extended_color_to_int%20%3D%20function%0A%20%20%7C%20%28%60RGBA%20%28r%2Cg%2Cb%2Ca%29%29%20-%3E%20256%20+%20a%20+%20b%20*%206%20+%20g%20*%2036%20+%20r%20*%20216%0A%20%20%7C%20%28%28%60Basic%20_%29%20%7C%20%28%60RGB%20_%29%29%20as%20col%20-%3E%20color_to_int%20col%0A%0A%3B%3B%0A%0Acolor_to_int%20%28%28%60Basic%20%28%28%60Black%29%2C%28%60Bold%29%29%29%29"></iframe>