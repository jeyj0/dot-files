{ stdenv, lib, pkgs }:

let
  repo = fetchGit {
    url = "https://codeberg.org/jeyj0/spare-actre.git";
    ref = "main";
    # rev = "";
  };
in
stdenv.mkDerivation rec {
  pname = "spare-actre";
  version = "0.0.1";

  src = repo;

  buildInputs = [ pkgs.cabal2nix ];

  buildPhase = ''
    cabal2nix . > default.nix
    nix-build release.nix
  '';

  installPhase = ''
    cp ./results/bin/spare-actre $out/bin/spare-actre
  '';

  meta = with lib; {
    description = "SPAced REpitition, ACTive REcall command-line app, ideally used with org-mode";
    homepage = "https://codeberg.org/jeyj0/spare-actre";
  };
}

