let
  pkgs = import ./nix/packages.nix {};
in
  pkgs.stdenv.mkDerivation {
    name = "battery-status";

    src = ./.;

    buildPhase = ''
      make build
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp out/main $out/bin/battery-status
    '';

    buildInputs = with pkgs; [
      ghc
      gnumake
    ];
  }
