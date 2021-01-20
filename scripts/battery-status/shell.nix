let
  pkgs = import ./nix/packages.nix {};
in
pkgs.mkShell {
  buildInputs = [
    pkgs.ghc
  ];
}
