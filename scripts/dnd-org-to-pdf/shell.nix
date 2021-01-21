let
  pkgs = import ./nix/packages.nix {};
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    niv

    ghc
    haskellPackages.haskell-language-server
  ];
}
