{ pkgs
, stdenv
}:
let
  ghc = pkgs.haskellPackages.ghcWithPackages (h: with h; [
    classy-prelude
    containers
    typerep-map
    neat-interpolation
    turtle
  ]);
in
stdenv.mkDerivation {
  name = "dotgen";
  src = ./.;
  buildPhase = ''
    make build
  '';
  installPhase = ''
    mkdir -p $out/bin
    cp out/main $out/bin/dotgen
  '';
  buildInputs = [
    ghc
    pkgs.gnumake
  ];
}

