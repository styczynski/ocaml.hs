all: interpreter
	echo "Done."

install-deps:
	stack install --only-dependencies

parser-src: install-deps
	stack exec bnfc -- -m -o parser ./grammar/syntax.cf
	sed -i -e 's/module Main where/module TestSyntax where/g' ./parser/TestSyntax.hs
	cd parser && stack exec happy -- -gca ParSyntax.y && stack exec alex -- -g LexSyntax.x && cd ..
	rm -f ocamlhs.cabal
	rm -r -f -d parser/*.x
	rm -r -f -d parser/*.y

interpreter: install-deps parser-src
	stack build

test: interpreter
	stack test :ocamlhs-test --no-terminal --coverage

clean:
	rm -r -f -d parser
