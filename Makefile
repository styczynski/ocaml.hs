all: interpreter web
	echo "Done."

format-code: install-deps
	stack exec brittany -- --write-mode=inplace ./src/**/*.hs

tintin:
	stack upgrade --allow-different-user --binary-version 2.1.1
	stack install --allow-different-user --only-dependencies
	stack install --allow-different-user tintin
	stack exec --allow-different-user tintin run

web: tintin interpreter-web
	echo "Done."

install-deps:
	stack upgrade --allow-different-user --binary-version 2.1.1
	stack install --allow-different-user --only-dependencies
	stack install --allow-different-user happy

parser-src: install-deps
	stack exec --allow-different-user bnfc -- -m -o parser ./grammar/syntax.cf
	sed -i -e 's/module Main where/module TestSyntax where/g' ./parser/TestSyntax.hs
	cd parser && stack exec --allow-different-user happy -- -gca ParSyntax.y && stack exec --allow-different-user alex -- -g LexSyntax.x && cd ..
	rm -f ocamlhs.cabal
	rm -r -f -d parser/*.x
	rm -r -f -d parser/*.y

interpreter: install-deps parser-src
	stack build --allow-different-user

interpreter-web:
	cd ./web && make all

test: interpreter generate-tests
	stack test --allow-different-user :ocamlhs-test --no-terminal --coverage

clean-tests:
	rm -r -f -d test/Generated

generate-tests: clean-tests generate-tests-good
	echo "Done. Generated all tests from examples."

generate-tests-good: $(wildcard examples/good/*.ml) $(subst .ml,GoodSpec.hs,$(subst examples/good/,,$(wildcard examples/good/*.ml)))
	echo "Generated tests from examples/good."

%GoodSpec.hs: examples/good/%.ml
	@mkdir -p test/Generated > /dev/null 2> /dev/null
	cat $< | stack exec --allow-different-user interpreter -- -g > $@.tmp
	$(eval NAME:=$(shell basename -s ".ml" "$<"))
	sed -i -e "s/module MainSpec/module Generated.$(NAME)GoodSpec/g" $@.tmp
	mv $@.tmp test/Generated/$@

clean:
	rm -r -f -d parser
