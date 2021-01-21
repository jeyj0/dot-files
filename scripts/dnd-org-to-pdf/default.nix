let
  pkgs = import ./nix/packages.nix {};
in
  pkgs.stdenv.mkDerivation {
    name = "dnd-org-to-tex";

    src = ./.;

    buildPhase = ''
      make build
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp out/main $out/bin/dnd-org-to-tex
    '';

    buildInputs = with pkgs; [
      ghc
      gnumake
    ];
  }
