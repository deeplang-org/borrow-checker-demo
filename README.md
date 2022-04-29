# Demo Implementation of Deeplang's Borrow Checker

## Building and Running
This repository has the following dependencies:

- `ocaml`, tested on `4.12.0`, but `4.7+` should be aqequate
- `dune 2.x`, tested on `2.8.5`

If you are using `nix`, you can use the `shell.nix` file and `nix-shell`
to setup the necessary dependencies.

There are also some optional dependencies:

- (WIP) `odoc`, for generating API document
- (WIP) `js_of_ocaml` and `js_of_ocaml-ppx`, for a HTML demo

To build the repository, do one of the following:
```
make build
dune build
```

Note that currently the HTML demo is not finished yet.
So there is nothing runnable,
only a rich set of tests and a library offering borrow checker functionalities.

## Document
API document is WIP.
Here's a brief introduction of the files:

- `Access.ml`: introducing the concept of "access",
with borrow constraint between access
and a data structure for a set of access constraints.
- `Type.ml`: definition of and operations on types in the demo language
- `Expr.ml`: definition of the demo language's expressions,
and the main borrow-checking function.
- `checker_test.ml`: a rich set of tests.
You can run them with `dune test` or `dune exec ./checker_test.exe`.
