.PHONY: all
all: build doc

.PHONY: build
build: checker demo

.PHONY: checker
checker:
	dune build checker

.PHONY: demo
demo: checker
	dune build demo
	install -m 644 _build/default/demo/checker_js.bc.js demo/checker.js

.PHONY: doc
doc: checker
	dune build @doc-private
	rm -Rf doc/api/*
	cp -r _build/default/_doc/_html/* doc/api

.PHONY: test
test: checker
	dune runtest

.PHONY: clean
clean:
	dune clean
