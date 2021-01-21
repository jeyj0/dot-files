let
  pkgs = import ./packages.nix {};
in
  pkgs.mkShell {
    buildInputs = with pkgs; [
      haskellPackages.brittany
    ];
  }
