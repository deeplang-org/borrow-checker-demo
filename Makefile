.PHONY: all
all: build doc

.PHONY: build
build:
	dune build

.PHONY: doc
doc: build
	dune build @doc-private
	rm -Rf doc/api/*
	cp -r _build/default/_doc/_html/* doc/api


.PHONY: clean
clean:
	dune clean
