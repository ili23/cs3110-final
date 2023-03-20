.PHONY: test check

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

# need to add check files
# check:
# 	@bash check.sh

# finalcheck:
# 	@bash check.sh final


zip:
	rm -f adventure.zip
	zip -r adventure.zip . -x@exclude.lst

clean:
	dune clean
	rm -f adventure.zip

# potentially need to add this stuff although if we dont make any compilation units, we prolly don't need this 
# doc:
# 	dune build @doc

# opendoc: doc
# 	@bash opendoc.sh
