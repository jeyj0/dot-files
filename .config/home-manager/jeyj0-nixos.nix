{ pkgs, ... }:
let
  userName = "jeyj0";
  userHome = "/home/jeyj0";
  packages = import ../../nixos/nix/packages.nix {};
in
{
  imports = [
    (import ./parts/linux.nix { pkgs = packages; })
    (import ./parts/common.nix { pkgs = packages; })
    (import ./parts/firefox {
      pkgs = packages;
      userName = userName;
    })
    (import ./parts/xmonad { pkgs = packages; })
    (import ./parts/latex { pkgs = packages; })
  ];

  home.packages = with pkgs; [
    # communication
    slack
    discord
    spotify
    zoom-us
  ];
}

