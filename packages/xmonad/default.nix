{ lib
, stdenv
, pkgs
, ...
}:
# stdenv.mkDerivation {
#   name = "xmonad";
#   src = ./.;
#   installPhase = ''
#     mkdir -p $out/bin
#     cat <<'EOF' >$out/bin/hello-xmonad
#     #!/bin/sh
#     echo "Hello, xmonad"
#     EOF
#     chmod +x $out/bin/hello-xmonad
#   '';
# }
pkgs.haskellPackages.callCabal2nix "xmonad" (
    pkgs.lib.sourceByRegex ./. [
      "xmonad.hs"
      "xmonad.cabal"
    ]
  ) {}
