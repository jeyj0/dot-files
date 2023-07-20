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
  ];

  home.packages = with pkgs; [
    # communication
    unstable.discord
    spotify
    zoom-us

    unstable.atkinson-hyperlegible
    unstable.kdenlive
    unstable.libwebp
  ];

  fonts.fontconfig.enable = true;
}

