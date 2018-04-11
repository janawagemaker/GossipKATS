.PHONY: default test clean frenetic-get-and-build

default:
	stack build

test:
	stack clean
	stack test --coverage

clean:
	stack clean
	rm -rf dist

frenetic-get-and-build:
	mkdir -p dist
	cd dist && git clone https://github.com/frenetic-lang/frenetic.git
	cd dist/frenetic && git checkout --track origin/verification_and_felix
	cd dist/frenetic && \
	  opam switch 4.03.0 && \
	  eval `opam config env` && \
		opam pin add frenetic . -n && \
		opam install --deps-only frenetic && \
		make
