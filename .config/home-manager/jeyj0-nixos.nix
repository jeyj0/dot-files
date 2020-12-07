{ pkgs, ... }:
let
  userName = "jeyj0";
  packages = import ../../nixos/nix/packages.nix {};
in
{
  imports = [
    (import ./parts/linux.nix { pkgs = packages; })
    (import ./parts/common.nix { pkgs = packages; })
    (import ./parts/kakoune { pkgs = packages; })
    (import ./parts/firefox {
      pkgs = packages;
      userName = userName;
    })
    (import ./parts/xmonad { pkgs = packages; })
  ];
}

