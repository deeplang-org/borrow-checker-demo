# Demo Implementation of Deeplang's Borrow Checker

## Building and Running
This repository has the following dependencies:

- `make`
- `ocaml`, tested on `4.12.0`, but `4.7+` should be aqequate
- `dune 2.x`, tested on `2.8.5`

If you are using `nix`, you can use the `shell.nix` file and `nix-shell`
to setup the necessary dependencies.

There are also some optional dependencies:

- `js_of_ocaml` and `js_of_ocaml-ppx`, for a HTML demo
- (WIP) `odoc`, for generating API document

To build this repository, do one of the following:
- `make`, to build everything.
- `make build`, to build everything except for API doc.
- `make checker`, to build the OCaml checker library only
- `make demo`, to build the HTML demo.
You can play with the demo in `demo/demo.html` afterwards.
- `make doc`, to build the API doc.

The HTML demo is also hosted on <https://guest0x0.xyz/deeplang-borrow-checker-demo/demo.html>.
You can play with it online there.

## Document
API document is WIP.
Here's a brief introduction of the files:

- `checker/Access.ml`: introducing the concept of "access",
with borrow constraint between access
and a data structure for a set of access constraints.
- `checker/Type.ml`: definition of and operations on types in the demo language
- `checker/Expr.ml`: definition of the demo language's expressions,
and the main borrow-checking function.
- `checker/checker_test.ml`: a rich set of tests.
You can run them with `make test`.
