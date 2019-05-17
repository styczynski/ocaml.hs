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

test: interpreter generate-tests
	stack test :ocamlhs-test --no-terminal --coverage

generate-tests: $(wildcard examples/*.ml) $(subst .ml,Spec.hs,$(subst examples/,,$(wildcard examples/*.ml)))
	echo "Generated tests from examples."

%Spec.hs: examples/%.ml
	@mkdir -p test/Generated > /dev/null 2> /dev/null
	cat $< | stack exec ocamlhs-exe -- -g > $@.tmp
	$(eval NAME:=$(shell basename -s ".ml" "$<"))
	echo $(NAME)
	$(info $(NAME))
	sed -i -e "s/module MainSpec/module Generated.$(NAME)Spec/g" $@.tmp
	mv $@.tmp test/Generated/$@

clean:
	rm -r -f -d parser
