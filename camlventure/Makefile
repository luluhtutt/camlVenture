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
	OCAMLRUNPARAM=b dune exec test/main.exe -- -runner sequential
	
bisect: bisect-clean
	-dune exec --instrument-with bisect_ppx --force test/main.exe -- -runner sequential
	bisect-ppx-report html
	open _coverage/index.html

bisect-clean:
	rm -rf _coverage bisect*.coverage

clean: bisect-clean
	dune clean
	rm -f camlventure.zip

play:
	dune exec camlventure

doc:
	dune build
	dune build @doc

opendoc: doc
	@bash opendoc.sh