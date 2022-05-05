with import <nixpkgs> {};

mkShell {
    buildInputs = [ nodejs nodePackages.inliner ] ++ (with ocamlPackages; [
        ocaml dune_2 findlib odoc
        js_of_ocaml js_of_ocaml-ppx
    ]);
}
