{ nixpkgs ? import ~/nixpkgs {}
}:
with nixpkgs;
let
  coq-serapi = ocamlPackages.callPackage ../PeaCoq/coq-serapi/default.nix {};
in
nixpkgs.mkShell {
  buildInputs = [
    # coq-serapi
    dune
    ocaml
  ] ++ (with ocamlPackages; [
    cmdliner
    findlib
    ppx_import
    ppx_sexp_conv
  ]);
}
