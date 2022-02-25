{ pkgs, ... }:
let
  userName = "jeyj0";
  userHome = "/home/jeyj0";
in
{
  imports = [
    (import ./parts/linux.nix { pkgs = pkgs; })
    (import ./parts/common.nix { pkgs = pkgs; })
    (import ./parts/firefox {
      pkgs = pkgs;
      userName = userName;
    })
    (import ./parts/xmonad { pkgs = pkgs; })
    (import ./parts/latex { pkgs = pkgs; })
    (import ./parts/games { pkgs = pkgs; })
  ];

  home.packages = with pkgs; [
    # communication
    slack
    unstable.discord
    spotify
    zoom-us
  ];
}

